module Imaging

(* abstrakt definition af et billede *)
// Et billede er en funktion fra et punkt i planen til en type (fx en farve).
type Point = float * float
type 'a Image = Point -> 'a
type Region = bool Image

(* helper functions for images *)
// approximate bool for region drawing; useful for drawing lines.
// essentially introduces a threshold for coloring a point on the region
// instead of only coloring exact points
let (<<>>) : float -> float -> bool =
  fun x y -> abs(y-x) < 0.05

// a helper function that computes the distance from point (x,y) to origo (0,0)
let distO ((x,y) : Point) : float =
  System.Math.Sqrt (x**2.0 + y**2.0)

// a helper function that asserts whether an integer is even
let even x = (x % 2 = 0)
// a helper function that can be used to emphasize characteristics of images
// by mapping floats to integers (ie. all points between two integers
// will map to the same color) 
let floori x = int (floor x)

(* images for drawing *)
// image of checker
let checker : Region =
  fun (x,y) -> even (floori x + floori y)

// image of alternating rings
let altRings : Region =
  even << floori << distO

// image of polar checker
type PolarPoint = float * float
let toPolar (x,y) : PolarPoint = (distO (x,y), atan2 y x)
let polarChecker n : Region =
  let sc (p, θ) = (p, θ * (float n) / System.Math.PI)
  checker << sc << toPolar

(* spatial transforms and applications (Filters) *)
// spatial transforms on points
type Transform = Point -> Point
type Vector = float * float
// translate point by vector
let translateP ((dx,dy) : Vector) : Transform =
  fun (x,y) -> (x + dx, y + dy)
// rotate point by angle θ
let rotateP θ : Transform =
  fun (x,y) -> (x * cos θ - y * sin θ, y * cos θ + x * sin θ)
// rotate point about the origin by an amount that depends on the 
// distance from the point to the origin (distO)
let swirlP radius : Transform =
  fun (x,y) -> rotateP (distO (x,y) * 2.0*System.Math.PI/radius) (x,y)

// The application of spatial transforms to images (Filters):
type 'a Filter = 'a Image -> 'a Image
// transforms should be applied like so (Elliott 2003, FoP p. 139)
//   transform << img^-1
// to avoid finding inverses, we instead compensate s.t.
//   img << transform
// produces the same effect.
// (it seems that making the arguments negative does the trick)
let translate (dx,dy) : 'a Filter =
  fun img -> img << translateP (-dx,-dy)
let rotate θ : 'a Filter =
  fun img -> img << rotateP (-θ)
let swirl radius : 'a Filter =
  // here we do not compensate by making the
  // angle θ in the swirlP function negative;
  // but what does it matter -- it's a swirl
  fun img -> img << swirlP (-radius)

(* pointwise lifitng *)
// lets us apply an operation (function op) pointwise on one or
// more function values
// pointwise operations: https://en.wikipedia.org/wiki/Pointwise

// definition of operations with arity 1-3
type unaryOp<'a,'b> = ('a -> 'b)
type binaryOp<'a,'b,'c> = ('a -> 'b -> 'c)
type ternaryOp<'a,'b,'c,'d> = ('a -> 'b -> 'c -> 'd)

let lift1 (op : unaryOp<'a,'b>) (f : 'p -> 'a) : 'p -> 'b =
  fun p -> op (f p)

let lift2 (op : binaryOp<'a,'b,'c>) (f1 : 'p -> 'a) f2 : 'p -> 'c =
  fun p -> op (f1 p) (f2 p)

let lift3 (op : ternaryOp<'a,'b,'c,'d>) (f1 : 'p -> 'a) f2 f3 : 'p -> 'd =
  fun p -> op (f1 p) (f2 p) (f3 p)

// the cond operator is quite powerful:
// given three regions r1, r2, and select. Use select to combine r1 and r2
// into a new region that is r1 whenever select would be and r2 otherwise.
// ie. we can select parts of images for arbitrary combinations,
// cropping etc.
let cond<'a> : Region -> 'a Image -> 'a Image -> 'a Image =
  lift3 (fun select r1 r2 -> if select then r1 else r2)

(* Region algebra *)
// pointwise lifting allows us to easily define region algebra
let universeR : Region = fun p -> true
let emptyR : Region = fun p -> false

let complementR : Region -> Region =
  lift1 (not)

let (<&&>) : Region -> Region -> Region =
  lift2 (&&)

// combine regions pointwise
let (<||>) : Region -> Region -> Region =
  lift2 (||)

let xorR : Region -> Region -> Region =
  lift2 (fun a b -> a && (not b) || (not a) && b)

let (</>) : Region -> Region -> Region =
  fun r r' -> r <&&> (complementR r')

(* pointwise filters *)
// extending upon the section about filters
let shiftXor (dx : float) : bool Filter =
  let shift d = translate (d,0.0)
  fun reg -> xorR (shift dx reg) (shift (-dx) reg)

(* color *)
type Frac = float // float in [0;1]
// BGRA color. BGR is multiplied by alpha; so alpha is upper bound
type Color = Frac * Frac * Frac * Frac // Blue Green Red Alpha
type ImageC = Color Image

let invisible : Color = (0.0, 0.0, 0.0, 0.0)
let black : Color = (0.0, 0.0, 0.0, 1.0)
let blue  : Color = (1.0, 0.0, 0.0, 1.0)
let green : Color = (0.0, 1.0, 0.0, 1.0)
let red   : Color = (0.0, 0.0, 1.0, 1.0)
let white : Color = (1.0, 1.0, 1.0, 1.0)

let toARGB ((b, g, r, a) : Color) =
  let conv x = int (255.0 * x) in
  conv a, conv r, conv g, conv b

let boolToColor c1 c2 b = if b then c1 else c2

let regionToImageC (img : Region) c1 c2 : ImageC =
  boolToColor c1 c2 << img

type Interval = struct
  // endpoints
  val A : float
  val B : float

  new (a, b) = { A = a; B = b }

  static member (<?>) (x : float, inter : Interval) =
    inter.A <= x && x <= inter.B
  static member (<?>) (p : Point, inter : Interval) =
    let x = fst p in
    let y = snd p in
    x <?> inter && y <?> inter
end

let imgInterval (inter : Interval) (img : ImageC) : ImageC =
  fun p -> if p <?> inter then img p else invisible

(* color manipulation *)
// interpolate between two colors by fractional amount
let lerpC (weight : Frac) (b1, g1, r1, a1) (b2, g2, r2, a2) : Color =
  let h x1 x2 = weight * x1 + (1.0 - weight) * x2 in
  (h b1 b2, h g1 g2, h r1 r2, h a1 a2)

let bilerpC wx wy c1 c2 c3 c4 : Color =
  let lerpa = lerpC wx c1 c2 in
  let lerpb = lerpC wx c3 c4 in
  lerpC wy lerpa lerpb

let lighten w : Color -> Color =
  lerpC w white

let darken w : Color -> Color =
  lerpC w black

let fade w : Color -> Color =
  lerpC w invisible

(* color images *)
// gradients defined for x in [0,1] and y in [0,1]
let gradient c1 c2 : ImageC =
  fun (x,y) -> lerpC x c1 c2
let gradient2D c1 c2 c3 c4 : ImageC =
  fun (x,y) -> bilerpC x y c1 c2 c3 c4

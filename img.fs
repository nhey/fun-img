open BitmapUtil

(* abstrakt definition af et billede *)
// Et billede er en funktion fra et punkt i planen til en type (fx en farve).
type Point = float * float
type 'a Image = Point -> 'a
type Region = bool Image

// approximate bool for region drawing; useful for drawing lines.
// essentially introduces a threshold for coloring a point on the region
// instead of only coloring exact points
let (<<>>) : float -> float -> bool =
  fun x y -> abs(y-x) < 0.05
// combine regions
let combine : Region -> Region -> Region =
  fun r r' -> fun (x,y) -> r (x,y) || r' (x,y)

// helper functions for images
let even x = (x % 2 = 0)
let floori x = int (floor x)

// images for drawing
let checker : Region =
  fun (x,y) -> even (floori x + floori y)

let distO ((x,y) : Point) : float =
  System.Math.Sqrt (x**2.0 + y**2.0)
let altRings : Region =
  even << floori << distO

// polar images
type PolarPoint = float * float
let toPolar (x,y) : PolarPoint = (distO (x,y), atan2 y x)
let polarChecker n : Region =
  let sc (p, θ) = (p, θ * (float n) / System.Math.PI)
  checker << sc << toPolar

// spatial transforms on points
type Transform = Point -> Point
type Vector = float * float
let translateP ((dx,dy) : Vector) : Transform =
  fun (x,y) -> (x + dx, y + dy)
let rotateP θ : Transform =
  fun (x,y) -> (x * cos θ - y * sin θ, y * cos θ + x * sin θ)
let swirlP radius : Transform =
  fun (x,y) -> rotateP (distO (x,y) * 2.0*System.Math.PI/radius) (x,y)
// The application of spatial transforms to images (Filters):
// transforms should be applied like
//   transform << img^-1
// to avoid finding inverses, we instead indvidually compensate s.t.
//   img << transform
// produces the same effect.
// (it seems that making the arguments negative does the trick)
type 'a Filter = 'a Image -> 'a Image
let translate (dx,dy) : 'a Filter =
  fun img -> img << translateP (-dx,-dy)
let rotate θ : 'a Filter =
  fun img -> img << rotateP (-θ)
let swirl radius : 'a Filter =
  // here we do not compensate by making the
  // angle θ in the swirlP function negative;
  // but what does it matter -- it's a swirl
  fun img -> img << swirlP (-radius)

// pointwise lifting
// lets us apply an operation (function o) pointwise on one or
// more function values
// pointwise operations: https://en.wikipedia.org/wiki/Pointwise
let lift1 (o : 'a -> 'b) (f : 'p -> 'a) : 'p -> 'b =
  fun p -> o (f p)

let lift2 (o : 'a -> 'b -> 'c) (f1 : 'p -> 'a) f2 : 'p -> 'c =
  fun p -> o (f1 p) (f2 p)

let lift3 (o : 'a -> 'b -> 'c -> 'd) (f1 : 'p -> 'a) f2 f3 : 'p -> 'd =
  fun p -> o (f1 p) (f2 p) (f3 p)

let cond : Region -> Region -> Region -> Region =
  lift3 (fun a b c -> if a then b else c)

// Region algebra

let universeR : Region = fun p -> true
let emptyR : Region = fun p -> false

let complementR : Region -> Region =
  lift1 (not)

let (<&&>) : Region -> Region -> Region =
  lift2 (&&)

let (<||>) : Region -> Region -> Region =
  lift2 (||)

let xorR : Region -> Region -> Region =
  lift2 (fun a b -> a && (not b) || (not a) && b)

let (</>) : Region -> Region -> Region =
  fun r r' -> r <&&> (complementR r')

let shiftXor (dx : float) : bool Filter =
  let shift d = translate (d,0.0)
  fun reg -> xorR (shift dx reg) (shift (-dx) reg)

(* konkretisering; fra abstrakt billede til bitmap *)
let regionToBitmap scale width height origoAnchor (img : Region) =
  // anchor (0,0) at center or top left
  let anchor (x,y) =
    match origoAnchor with
    | BitmapUtil.Center -> (x - width/2, y - height/2)
    | BitmapUtil.TopLeft -> (x,y)

  // construct bitmap
  let bmp = BitmapUtil.bmp (width, height)
  for x in [0..width-1] do
    for y in [0..height-1] do
      let ax, ay = anchor (x,y)
      // apply scale
      let p_x = float ax * scale / float width
      let p_y = float ay * scale / float height
      let color = img (p_x, p_y) |> BitmapUtil.boolToColor
      in BitmapUtil.setPixel color (x,y) bmp
  bmp

// shiftXor 2.6 altRings
// swirl 333.0 (shiftXor 2.6 altRings)
// swirl 150.0 (shiftXor 2.6 altRings)
// shiftXor 2.6 altRings
// cond (fun (x,y) -> (y - x) < 0.0) checker (swirl -33.3 checker)
// |> regionToBitmap 20.0 600 600 Center
// |> regionToBitmap 400.0 600 600 TopLeft
// |> toPngFile "test.png"
// printfn "File written: 'test.png'"

// Animationer
type Time = float
type 'a Animation = Time -> 'a Image
// Write multiple images
printfn "Generating images..."
let anim : bool Animation =
  fun dt -> shiftXor (0.0 + dt) altRings

let generateImgi =
  fun i ->
    anim (float i * 0.05)
    |> regionToBitmap 400.0 600 600 TopLeft

let images = Array.Parallel.init 80 generateImgi

printfn "Writing images to disk..."
Array.iteri (fun i img -> toPngFile (sprintf "test%02i.png" i) img) images

// ImageMagick for gif generation
printfn "Creating gif with ImageMagick..."
let makeGif = "-delay 10 -loop 0 test*.png test.gif"
let patrolCycle =  "test.gif -coalesce -duplicate 1,-2-1 -quiet -layers OptimizePlus  -loop 0 test.gif"
let convert cmd =
  use p = System.Diagnostics.Process.Start("convert", cmd)
  p.WaitForExit()
convert makeGif
// convert patrolCycle
printfn "File written: test.gif"

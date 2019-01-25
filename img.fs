open BitmapUtil

(* abstrakt definition af et billede *)
// Et billede er en funktion fra et punkt i planen til en type (fx en farve).
type Point = float * float
type 'a Image = Point -> 'a
type Region = bool Image

// approximate to bool
let (<<>>) : float -> float -> bool =
  fun x y -> abs(y-x) < 0.05
// combine regions
let (<||>) : Region -> Region -> Region =
  fun r r' -> fun (x,y) -> r (x,y) || r' (x,y)

// images for drawing
let even x = (x % 2 = 0)
let floori x = int (floor x)
let checker : Region =
  fun (x,y) -> even (floori x + floori y)

let distO ((x,y) : Point) : float =
  System.Math.Sqrt (x**2.0 + y**2.0)
let altRings : Region =
  even << floori << distO

// polar
type PolarPoint = float * float
let toPolar (x,y) : PolarPoint = (distO (x,y), atan2 y x)
let polarChecker n : Region =
  let sc (p, θ) = (p, θ * (float n) / System.Math.PI)
  checker << sc << toPolar

// spatial transforms
type Transform = Point -> Point
type Vector = float * float
let translateP ((dx,dy) : Vector) : Transform =
  fun (x,y) -> (x + dx, y + dy)
let rotateP θ : Transform =
  fun (x,y) -> (x * cos θ - y * sin θ, y * cos θ + x * sin θ)
let swirlP r : Transform =
  fun (x,y) -> rotateP (distO (x,y) * 2.0*System.Math.PI/r) (x,y)
// transforms should be applied like
//   transform << img^-1
// to avoid finding inverses, we instead indvidually compensate s.t.
//   img << transform
// produces the same effect
type 'a Filter = 'a Image -> 'a Image
let translate (dx,dy) : 'a Filter =
  fun img -> img << translateP (-dx,-dy)
let rotate θ : 'a Filter =
  fun img -> img << rotateP (-θ)
let swirl r : 'a Filter =
  // here it is apparant that we don't make the angle θ negative
  // but what does it matter -- it's a swirl
  fun img -> img << swirlP (-r)

(* konkretisering; fra abstrakt billede til bitmap *)
let toBitmap (img : Region) scale width height =
  let bmp = BitmapUtil.bmp (width, height)
  for x in [0..width-1] do
    for y in [0..height-1] do
      // centreer omkring (0,0)
      let p_x = float (x - width/2) * scale / float width
      let p_y = float (y - height/2) * scale / float height
      let c = img (p_x, p_y) |> BitmapUtil.boolToColor
      in BitmapUtil.setPixel c (x,y) bmp
  bmp

toBitmap (swirl 33.3 checker) 10.0 600 600
|> toPngFile "test.png"

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
let (<||>) : Region -> Region -> Region =
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
let swirlP r : Transform =
  fun (x,y) -> rotateP (distO (x,y) * 2.0*System.Math.PI/r) (x,y)
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
let swirl r : 'a Filter =
  // here we do not compensate by making the
  // angle θ in the swirlP function negative;
  // but what does it matter -- it's a swirl
  fun img -> img << swirlP (-r)

(* konkretisering; fra abstrakt billede til bitmap *)
let regionToBitmap (img : Region) scale width height =
  let bmp = BitmapUtil.bmp (width, height)
  for x in [0..width-1] do
    for y in [0..height-1] do
      // centreer omkring (0,0)
      let p_x = float (x - width/2) * scale / float width
      let p_y = float (y - height/2) * scale / float height
      let c = img (p_x, p_y) |> BitmapUtil.boolToColor
      in BitmapUtil.setPixel c (x,y) bmp
  bmp

regionToBitmap (swirl 33.3 checker) 60.0 600 600
|> toPngFile "test.png"
printfn "File written: 'test.png'"

// for i in [0..80] do
//   regionToBitmap (checker |> swirl (float (40-i))) 10.0 600 600
//   |> toPngFile (sprintf "test%02i.png" i)
// // ImageMagick for gif generation
// let makeGif = "-delay 10 -loop 0 *.png test.gif"
// let patrolCycle =  "test.gif -coalesce -duplicate 1,-2-1 -quiet -layers OptimizePlus  -loop 0 test.gif"
// let conv cmd =
//   use p = System.Diagnostics.Process.Start("convert", cmd)
//   p.WaitForExit()
// conv makeGif
// conv patrolCycle

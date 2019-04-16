open Imaging
open BitmapUtil

(* konkretisering; fra abstrakt billede til bitmap *)
let regionToBitmap scale width height origoAnchor (img : Imaging.Region) =
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

let toBitmap scale width height origoAnchor (img : Imaging.ImageC) =
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
      let color = img (p_x, p_y) |> Imaging.toARGB |> BitmapUtil.fromARGB
      in BitmapUtil.setPixel color (x,y) bmp
  bmp

// cond checker (gradient cBlue cGreen) (gradient cGreen cRed)
// gradient cBlue cRed
// Imaging.regionToImageC checker cRed (fade 0.0 cRed)
// let cross = regionToImageC (fun (x,y) -> x <<>> y) black white
// lift2 (&&) cross (imgInterval (new Interval(0.0,1.0)) (gradient2D black red blue white))
// swirl 1.0 (imgInterval (new Interval(0.0,1.0)) (gradient2D black red blue white))
cond checker (swirl 1.0 (imgInterval (new Interval(0.0,1.0)) (gradient2D black (lighten 0.7 red) green white))) (swirl 1.0 (imgInterval (new Interval(0.0,1.0)) (gradient2D black red blue white)))
// gradient black red
// |> toBitmap 1.0 600 600 TopLeft
|> toBitmap 4.0 600 600 Center
// // swirl 333.0 (shiftXor 2.6 altRings)
// // swirl 150.0 (shiftXor 2.6 altRings)
// shiftXor 2.6 altRings
// // cond (fun (x,y) -> (y - x) < 0.0) checker (swirl -33.3 checker)
// // |> regionToBitmap 20.0 600 600 Center
// |> regionToBitmap 400.0 600 600 TopLeft
|> toPngFile "test.png"
printfn "File written: 'test.png'"

// // Animationer
// type Time = float
// type 'a Animation = Time -> 'a Image
// // Write multiple images
// printfn "Generating images..."
// let anim : bool Animation =
//   fun dt -> shiftXor (0.0 + dt) altRings

// let generateImgi =
//   fun i ->
//     anim (float i * 0.05)
//     |> regionToBitmap 400.0 600 600 TopLeft

// let images = Array.Parallel.init 80 generateImgi

// printfn "Writing images to disk..."
// Array.iteri (fun i img -> toPngFile (sprintf "test%02i.png" i) img) images

// // ImageMagick for gif generation
// printfn "Creating gif with ImageMagick..."
// let makeGif = "-delay 10 -loop 0 test*.png test.gif"
// let patrolCycle =  "test.gif -coalesce -duplicate 1,-2-1 -quiet -layers OptimizePlus  -loop 0 test.gif"
// let convert cmd =
//   use p = System.Diagnostics.Process.Start("convert", cmd)
//   p.WaitForExit()
// convert makeGif
// // convert patrolCycle
// printfn "File written: test.gif"

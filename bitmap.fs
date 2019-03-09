(* Med udgangspunkt i Martin Elsmans img_util *)
module BitmapUtil
open System.Drawing

(* color *)
let fromArgb (a:int, r:int, g:int, b:int) : Color =
  Color.FromArgb(a,r,g,b)

let boolToColor (c : bool) : Color =
  if c then Color.Black
  else Color.White

(* imaging *)
let toPngFile (fname : string) (bmp : Bitmap) =
  bmp.Save(fname, Imaging.ImageFormat.Png) |> ignore

let bmp ((width, height) : int * int) : Bitmap = new Bitmap(width, height)
let setPixel (c : Color) (x,y) (bmp : Bitmap) = bmp.SetPixel(x,y,c)

type Anchor =
  | Center
  | TopLeft


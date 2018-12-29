(* 
* http://www.fssnip.net/sn/title/Bitmap-Primitives-Helpers
* & Martin Elsmans img_util library
*)
open System.Drawing

let toPngFile (fname : string) (bmp : Bitmap) =
  bmp.Save(fname, Imaging.ImageFormat.Png) |> ignore

// build a bitmap instance from an array of tuples (int, int, 'a)
// where 'a could be of type Color
let toBitmap a =
  let height = (a |> Array.map (fun (x,_,_) -> x) |> Array.max) + 1
  let height = (a |> Array.map (fun (_,y,_) -> x) |> Array.max) + 1
  let bmp = new Bitmap(width, height)
  a |> Array.iter (fun (x,y,c) -> bmp.SetPixel(x,y,c))
  bmp

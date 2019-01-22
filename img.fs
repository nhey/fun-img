open BitmapUtil

(* abstrakt definition af et billede *)
// Et billede er en funktion fra et punkt i planen til en type (fx en farve).
type Point = float * float
type 'a Image = Point -> 'a
type Region = bool Image

let even x = (x % 2 = 0)
let floori x = int (floor x)
let checker : Region =
  fun (x,y) -> even (floori (abs x) + floori (abs y))

let (<<>>) : float -> float -> bool =
  fun x y -> abs(y-x) < 0.05

let (<||>) : Region -> Region -> Region =
  fun r r' -> fun (x,y) -> r (x,y) || r' (x,y)

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

toBitmap checker 10.0 600 600
|> toPngFile "test.png"

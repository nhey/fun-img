type Point = (float, float)
type Image 'a = Point -> 'a
type Region = Image Bool

let even x = (x % 2 = 0)
let checker (x,y) = even(abs(x) + abs(y))
// nu lav array af (x,y,color) sort/hvid og toPngFile

type PolarPoint = (float, float)

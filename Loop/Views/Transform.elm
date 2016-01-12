module Loop.Views.Transform where

import Loop.Disk exposing ( Disk )

type alias Point = (Float,Float)
type alias Radius = Float
type alias Center = Point


cartesian : Disk -> (Int,Int) -> Point
cartesian disk (radial, concentric) =
    let 
        modulus = (toFloat radial) * (2 * pi / toFloat disk.radialDivisions) + (pi / toFloat disk.radialDivisions)
        argument = (toFloat concentric) * (1 / toFloat disk.concentricDivisions ) + 1 / (2 * toFloat disk.concentricDivisions)
    in 
        ( argument * cos modulus, argument * sin modulus )



affine : Center -> Radius -> Point -> Point
affine (cx,cy) r (x,y) = (cx + r * x, cy + r * y )
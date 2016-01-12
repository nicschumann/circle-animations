module Loop.Views.Linear where

import List 
import String

import Loop.Disk exposing ( Disk )
import Loop.Edge exposing ( Edge )

import Loop.Views.Transform exposing ( Center, Radius, Point, affine, cartesian)
import Loop.Views.Render exposing ( RenderStepFunction )



draw : RenderStepFunction
draw c r disk edge path =
    let
        targetPoint = ((affine c r) << (cartesian disk)) edge.end
        nextPathState = svgPathComponentString "L" targetPoint
    in
        { pathState = path.pathState ++ " " ++ nextPathState
        , previousPoints = targetPoint :: path.previousPoints
        }


svgPathComponentString : String -> Point -> String
svgPathComponentString command (x,y) = String.join " " [command, toString x, toString y]



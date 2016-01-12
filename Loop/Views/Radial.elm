module Loop.Views.Radial where

import List 
import String
import Basics exposing (pi, sin, cos)
import Maybe exposing (withDefault, andThen)

import Loop.Disk exposing ( Disk )
import Loop.Edge exposing ( Edge, isRadialEdge, isConcentricEdge )

import Loop.Views.Transform exposing ( Center, Radius, Point, affine, cartesian)
import Loop.Views.Render exposing ( RenderStepFunction )



draw : RenderStepFunction
draw c radius disk edge path =
    let
        targetPoint = ((affine c radius) << (cartesian disk)) edge.end
        nextPathState = 
            if isConcentricEdge edge then
                let 
                    r = radius * ((toFloat <| snd edge.start) * (1.0 / (toFloat disk.concentricDivisions)) + 1.0 / (2.0 * (toFloat disk.concentricDivisions)))

                    theta1 = (toFloat <| fst edge.start) * (2.0 * pi / (toFloat disk.radialDivisions)) + pi / (toFloat disk.radialDivisions)
                    theta2 = (toFloat <| fst edge.end) * (2.0 * pi / (toFloat disk.radialDivisions)) + pi / (toFloat disk.radialDivisions)

                in
                    svgArcComponentString r theta1 (clockwise theta1 theta2) targetPoint

            else if isRadialEdge edge then

                svgLinePathComponentString "L" targetPoint

            else 
                svgLinePathComponentString "L" targetPoint
    in
        { pathState = path.pathState ++ " " ++ nextPathState
        , previousPoints = targetPoint :: path.previousPoints
        }


svgLinePathComponentString : String -> Point -> String
svgLinePathComponentString command (x,y) = String.join " " [command, toString x, toString y]


svgArcComponentString : Radius -> Float -> Int -> Point -> String
svgArcComponentString radius angle direction (x,y) =
    String.join " " ["A", toString radius, toString radius, toString angle, toString 0, toString direction, toString x, toString y ]

clockwise : Float -> Float -> Int
clockwise t1 t2 = if sin ( t2 - t1 ) > 0 then 1 else 0




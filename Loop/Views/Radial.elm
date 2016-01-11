module Loop.Views.Radial where

import List 
import String
import Basics exposing (pi, sin, cos)
import Maybe exposing (withDefault, andThen)

import Html exposing ( Html )
import Svg exposing ( Svg, svg )
import Svg.Attributes exposing ( d, class, width, height )
import Svg.Lazy exposing ( lazy3 )

import Loop.Disk exposing ( Disk )
import Loop.Edge exposing ( Edge, isRadialEdge, isConcentricEdge )

import Loop.Views.Transform exposing (IntermediatePath, Center, Radius, Point, affine, cartesian)


initialPath : Center -> Radius -> Disk -> IntermediatePath
initialPath c r disk = 
    let 
        firstPoint = ((affine c r) << (cartesian disk)) (withDefault (0,0) ((List.head disk.loop) `andThen` (\x -> Just x.start)))
        firstPathState = svgLinePathComponentString "M" firstPoint
    in
        { pathState = firstPathState
        , previousPoints = [ firstPoint ]
        }


view : Center -> Radius -> Disk -> Html
view c r disk = svg [width "1000", height "1000"] [lazy3 renderDisk c r disk]


renderDisk : Center -> Radius -> Disk -> Svg
renderDisk c r disk =
    let 
        pathComponent = renderPathStep c r disk 
        intermediatePath = (List.foldl pathComponent (initialPath c r disk) disk.loop)
        svgPath = intermediatePath.pathState ++ " z"
    in 
        Svg.path [d svgPath, class "loop"] []


renderPathStep : Center -> Radius -> Disk -> Edge -> IntermediatePath -> IntermediatePath
renderPathStep c radius disk edge path =
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




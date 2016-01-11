module Loop.Views.Linear where

import List 
import String
import Basics exposing (pi, sin, cos)
import Maybe exposing (withDefault, andThen)

import Html exposing ( Html )
import Svg exposing ( Svg, svg )
import Svg.Attributes exposing ( d, class, width, height )
import Svg.Lazy exposing ( lazy3 )

import Loop.Disk exposing ( Disk )
import Loop.Edge exposing ( Edge )

import Loop.Views.Transform exposing (IntermediatePath, Center, Radius, Point, affine, cartesian)


initialPath : Center -> Radius -> Disk -> IntermediatePath
initialPath c r disk = 
    let 
        firstPoint = ((affine c r) << (cartesian disk)) (withDefault (0,0) ((List.head disk.loop) `andThen` (\x -> Just x.start)))
        firstPathState = svgPathComponentString "M" firstPoint
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
renderPathStep c r disk edge path =
    let
        targetPoint = ((affine c r) << (cartesian disk)) edge.end
        nextPathState = svgPathComponentString "L" targetPoint
    in
        { pathState = path.pathState ++ " " ++ nextPathState
        , previousPoints = targetPoint :: path.previousPoints
        }


svgPathComponentString : String -> Point -> String
svgPathComponentString command (x,y) = String.join " " [command, toString x, toString y]



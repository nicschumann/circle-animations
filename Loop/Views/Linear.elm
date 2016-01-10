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

type alias Point = (Float,Float)
type alias Radius = Float
type alias Center = Point

type alias IntermediatePath = 
    { pathState : String
    , previousPoints : List Point 
    }

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


cartesian : Disk -> (Int,Int) -> Point
cartesian disk (radial, concentric) =
    let 
        modulus = (toFloat radial) * (2 * pi / toFloat disk.radialDivisions) + (pi / toFloat disk.radialDivisions)
        argument = (toFloat concentric) * (1 / toFloat disk.concentricDivisions ) + 1 / (2 * toFloat disk.concentricDivisions)
    in 
        ( argument * cos modulus, argument * sin modulus )



affine : Center -> Radius -> Point -> Point
affine (cx,cy) r (x,y) = (cx + r * x, cy + r * y )



module Loop.Views.Render where
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
import Loop.Views.Transform exposing ( affine, cartesian, Point, Center, Radius )

type alias Point = (Float,Float)
type alias Radius = Float
type alias Center = Point

type alias IntermediatePath = 
    { pathState : String
    , previousPoints : List Point 
    }

type alias RenderStepFunction = Center -> Radius -> Disk -> Edge -> IntermediatePath -> IntermediatePath

view : RenderStepFunction -> Center -> Radius -> Disk -> Svg
view f c r disk = 
    let
        step = render f
    in
        lazy3 step c r disk


render : RenderStepFunction -> Center -> Radius -> Disk -> Svg
render f c r disk =
    let 
        pathComponent = f c r disk 
        intermediatePath = (List.foldl pathComponent (initial c r disk) disk.loop)
        svgPath = intermediatePath.pathState ++ " z"
    in 
        Svg.path [d svgPath, class "loop"] []



initial : Center -> Radius -> Disk -> IntermediatePath
initial c r disk = 
    let 
        firstPoint = ((affine c r) << (cartesian disk)) (withDefault (0,0) ((List.head disk.loop) `andThen` (\x -> Just x.start)))
        firstPathState = String.join " " ["M", toString <| fst firstPoint, toString <| snd firstPoint]
    in
        { pathState = firstPathState
        , previousPoints = [ firstPoint ]
        }










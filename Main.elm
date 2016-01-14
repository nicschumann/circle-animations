
import Time exposing ( Time, every, second, minute, timestamp )

import Mouse
import Window

import Signal exposing (Signal, map, mergeMany, foldp)
import Signal.Extra exposing (foldp')

import Html exposing ( Html )
import Svg exposing (svg)
import Svg.Attributes exposing (width, height)

import Loop.Disk exposing (Disk)

import Loop.RandomDisk as Disk

import Loop.Views.Radial as Radial
import Loop.Views.Render as Render

type alias Width = Int

type alias Height = Int

type alias Model = (Disk.RandomDisk, (Width,Height))

type Update = Tick Time | Reset Time | Resize Time (Width,Height)

radial : Int
radial = 20

concentric : Int
concentric = 20

initial : (Width,Height)
initial = (1000,1000)

feed : Signal Update
feed =
    mergeMany 
        [ map (\(t,d) -> Resize t d) (timestamp Window.dimensions)
        , (map Reset << map fst) (timestamp Mouse.clicks)
        , map Tick (every (minute / 136))
        ]


update : Signal Model
update =
    let
        buildInitial update =
            case update of 
                Tick t -> 
                    (Disk.initial t (radial,concentric), initial) 

                Reset t ->
                    (Disk.initial t (radial,concentric), initial) 

                Resize t dim ->
                    (Disk.initial t (radial,concentric), dim) 

    in
        foldp' buildStep buildInitial feed



buildStep : Update -> Model -> Model
buildStep update (randomDisk, (w,h)) =
    case update of 
        Tick t ->
            let 
                new = Disk.update randomDisk

            in 
                if .loop (fst new) /= .loop (fst randomDisk) then

                   (new, (w,h))

                else 

                    buildStep (Reset t) (randomDisk,(w,h))

            

        Reset t ->
            (Disk.initial t (radial,concentric), (w,h))

        Resize t dim ->
            (randomDisk, dim)



render : Model -> Html
render ((disk,seed),(wI,hI)) =
    let 
        w = toFloat wI 
        h = toFloat hI
    in
        svg [width (toString w), height (toString h)] [Render.view Radial.draw (w/2.0,h/2.0) ((min w h)/2.0) disk]


main : Signal Html
main =

    map render update


















import Time exposing ( Time, every, second, timestamp )

import Mouse

import Signal exposing (Signal, map, merge, foldp)

import Html exposing ( Html )

import Loop.Disk exposing (Disk)

import Loop.RandomDisk as Disk

import Loop.Views.Linear as Linear


type alias Model = Disk.RandomDisk

radial : Int
radial = 10

concentric : Int
concentric = 15


type Update = Tick Time | Reset Time


feed : Signal Update
feed =
    merge 
        (map Reset (map fst (timestamp Mouse.clicks)))
        (map Tick (every (second / 12)))


update : Signal Model
update =
    foldp step (Disk.initial 400 (radial,concentric)) feed


step : Update -> Model -> Model
step update randomDisk =
    case update of 
        Tick t ->
            Disk.update randomDisk

        Reset t ->
            Disk.initial t (radial,concentric)


render : Model -> Html
render (disk,seed) =
    Linear.view (500,500) 300 disk


main : Signal Html
main =
    map render update
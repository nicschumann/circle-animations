module Loop.Disk where

import List 
import Result exposing ( Result( Ok, Err ) )
import Maybe exposing ( Maybe(Just, Nothing) )

import Loop.Edge exposing (Edge, edge, isConcentricEdge, isRadialEdge, intersects)

type alias Disk = 
    { radialDivisions : Int
    , concentricDivisions : Int
    , loop : List Edge
    }



initial : (Int, Int) -> Int -> Disk
initial (radial,concentric) starting =
    { radialDivisions = radial
    , concentricDivisions = concentric
    , loop = initialLoop (radial,concentric) starting
    }


initialLoop : (Int, Int) -> Int -> List Edge
initialLoop (radial,concentric) starting =
    let
        ring position = edge ( position, starting ) ( (position + 1) % radial, starting )
    in 
        List.map ring [0..(radial - 1)] 


inDiskLoop : Disk -> Edge -> Bool
inDiskLoop disk edge = 
    List.member edge disk.loop


isFreeEdge : Disk -> Edge -> Bool
isFreeEdge disk edge =
    let
        inLoop = inDiskLoop disk 
    in
        ((List.foldl (||) False) << (List.map (\neighbor -> not (inLoop neighbor) ))) (freeNeighbors disk edge)


neighbors : Disk -> Edge -> List Edge
neighbors disk edge =
    let
        concentricNeighbors edge = 
            let 
                (startR,startC) = edge.start 
                (endR,endC) = edge.end
            in
                if startC == 0 then 
                    [ Edge (startR, 1) (endR, 1) ]

                else if startC == disk.concentricDivisions - 1 then 
                    [ Edge (startR, startC - 1) (endR, startC - 1) ]

                else 
                    [ Edge ( startR, startC + 1 ) (endR, endC + 1)
                    , Edge ( startR, startC - 1 ) (endR, endC - 1)
                    ]

        radialNeighbors edge =
            let 
                (startR,startC) = edge.start 
                (endR,endC) = edge.end

                circular value = 
                    (disk.radialDivisions + ( value % disk.radialDivisions )) % disk.radialDivisions
            in
                [ Edge (circular (startR + 1), startC) (circular (endR + 1), endC)
                , Edge (circular (startR - 1), startC) (circular (endR - 1), endC)
                ]

    in
        if  isConcentricEdge edge then 
            concentricNeighbors edge

        else if isRadialEdge edge then 
            radialNeighbors edge

        else 
            []

freeNeighbors : Disk -> Edge -> List Edge
freeNeighbors disk edge =
    let 
        intersections edge1 = List.isEmpty (List.filter (\edge2 -> intersects edge1 edge2) disk.loop)
    in
        List.filter intersections (neighbors disk edge)


type Manipulation = Pull Edge Edge

update : Disk -> Manipulation -> (Disk, Result String ())
update disk manipulation = 
    case manipulation of

        Pull edgeFrom edgeTo ->
            case connect disk edgeFrom edgeTo of
                Just newDisk -> 
                    (newDisk, Ok ())

                Nothing -> 
                    (disk, Err "Looks like we tried to pull an edge into a non-neighboring position!")


connect : Disk -> Edge -> Edge -> Maybe Disk
connect disk start end =
    if validPull disk start end then 
        let 
            ( before, after ) = divide disk.loop start
        in
            Just { disk |
                loop = before ++ (pull start end) ++ after
            }

    else 
        Nothing

divide : List Edge -> Edge -> (List Edge, List Edge)
divide list edge =
    let 
        indexTest (m,e) n = if edge == e then m else n
        targetIndex = ((List.foldl indexTest 0) << List.indexedMap (,)) list
        before = List.take targetIndex list
        after = List.drop (targetIndex + 1) list
    in 
        ( before, after )

validPull : Disk -> Edge -> Edge -> Bool
validPull disk edge1 edge2 = List.member edge2 (neighbors disk edge1)


pull : Edge -> Edge -> List Edge
pull start end = [ edge start.start end.start, end, edge end.end start.end ]







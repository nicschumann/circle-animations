module Loop.RandomDisk where


import List

import Array

import Maybe

import Time exposing (Time)

import Random exposing (Seed, int, generate, initialSeed)

import Loop.Disk as Disk

import Loop.Edge as Edge

import Debug


type alias RandomDisk = (Disk.Disk,Seed)


initial : Time -> (Int,Int) -> RandomDisk
initial time (radial,concentric) =
    let 
        generator = int 0 (radial-1)

        seed0 = initialSeed (round time)

        (start,seed1) = generate generator seed0

    in

        (Disk.initial (radial,concentric) start, seed1)


update : RandomDisk -> RandomDisk
update (disk,seed1) =
    let
        errorEdge = (Edge.edge (0,0) (0,0))

        freeSet = (Array.fromList (List.filter (Disk.isFreeEdge disk) disk.loop))

        generator1 = int 0 ((Array.length freeSet) - 1)

        (index1,seed2) = generate generator1 seed1

        sourceChoice = Maybe.withDefault errorEdge (Array.get index1 freeSet)

        neighborSet = Array.fromList (Disk.freeNeighbors disk sourceChoice)

        generator2 = int 0 ((Array.length neighborSet) - 1)

        (index2,seed3) = generate generator2 seed2

        targetChoice = Maybe.withDefault errorEdge (Array.get index2 neighborSet)

    in 

        (fst (Disk.update disk (Disk.Pull sourceChoice targetChoice)), seed3) 
      



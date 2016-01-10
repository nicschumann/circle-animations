module Loop.Edge where

type alias Radial = Int
type alias Concentric = Int

type alias Edge =
    { start : (Radial,Concentric)
    , end : (Radial,Concentric)
    }

edge : (Radial,Concentric) -> (Radial,Concentric) -> Edge
edge startsAt endsAt =
    { start = startsAt
    , end = endsAt
    }


isLoop : Edge -> Bool
isLoop edge =
    edge.start == edge.end

isConcentricEdge : Edge -> Bool
isConcentricEdge edge =
    let 
        concentric (_,c1) (_,c2) = c1 == c2
    in 
        (not (isLoop edge)) && (concentric edge.start edge.end)

isRadialEdge : Edge -> Bool
isRadialEdge edge = 
    let 
        radial (r1,_) (r2,_) = r1 == r2
    in 
        (not (isLoop edge)) && (radial edge.start edge.end)

intersects : Edge -> Edge -> Bool
intersects edge1 edge2 =
    edge1.start == edge2.start 
    || edge1.start == edge2.end 
    || edge1.end == edge2.start 
    || edge1.end == edge2.end
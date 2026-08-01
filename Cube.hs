module Cube where

import Data.Set (Set)
import Data.List    


import Data.Sequence (Seq, ViewL(..), (|>), viewl)
import qualified Data.Set as Set
--           ( X , Y  , Z  )
type Point = (Int, Int, Int)

type Orientation = Int

data Piece = Red     (Set Point) Orientation 
           | Orange  (Set Point) Orientation 
           | Blue    (Set Point) Orientation 
           | Edge Int(Set Point) Orientation
           | Corner  (Set Point) Orientation
    deriving (Eq, Show, Ord)

data Move = U | D | B | F | L | R
    deriving (Eq, Show, Ord)


type Cube =  Set Piece

solvedCube :: Cube

solvedCube = Set.fromList [-- x y z
        Corner (Set.fromList [(2,2,2)]) 0,
        Edge 0 (Set.fromList [(2,1,2)]) 0,
        Edge 1 (Set.fromList [(1,2,2)]) 0,
        Edge 2 (Set.fromList [(2,2,1)]) 0,

        Red    (Set.fromList [(1,0,2),(2,0,1),(2,0,2)]) 0,
        Red    (Set.fromList [(0,1,2),(0,2,2),(0,2,1)]) 0,
        Red    (Set.fromList [(1,2,0),(2,2,0),(2,1,0)]) 0,

        Orange (Set.fromList [(0,0,1),(0,0,2)]) 0,
        Orange (Set.fromList [(0,1,0),(0,2,0)]) 0,
        Orange (Set.fromList [(1,0,0),(2,0,0)]) 0,

        Blue   (Set.fromList [(0,0,0)]) 0 
       ]
-------------------------------------------------------------------------------


move :: Cube -> Move -> Cube
move cube m = Set.map (turn m) cube

---- does not validate a move
turn m piece | any (affects m) (pointsOf piece) = rotate piece m
             | otherwise                        = piece

possibleMove :: Cube -> [Move]
possibleMove cube = filter (valid . move cube) [U,D,R,L,F,B]

neighbours :: Cube -> [(Cube ,Move)]
neighbours cube = [ (c,m) | m <- [U,D,R,L,F,B], let c = move cube m, valid c ]

valid :: Cube -> Bool 
-- valid = (== 20) . Set.size . Set.unions . Set.map pointsOf 
valid  = (== 20) . Set.size . foldMap pointsOf

affects :: Move -> Point -> Bool
affects U (_,_,z) = z == 2
affects D (_,_,z) = z == 0
affects R (x,_,_) = x == 2
affects L (x,_,_) = x == 0
affects F (_,y,_) = y == 2
affects B (_,y,_) = y == 0

pointsOf :: Piece -> Set Point
pointsOf (Corner l _) = l
pointsOf (Edge _ l _) = l
pointsOf (Red    l _) = l
pointsOf (Orange l _) = l
pointsOf (Blue   l _) = l

rotate :: Piece -> Move -> Piece
rotate p@(Corner l _) m = Corner (rotateCords m l) (orient p m)
rotate p@(Edge i l _) m = Edge i (rotateCords m l) (orient p m)
rotate   (Red    l o) m = Red    (rotateCords m l) o
rotate   (Orange l o) m = Orange (rotateCords m l) o
rotate   (Blue   l o) m = Blue   (rotateCords m l) o

axisOf :: Move -> Int
axisOf U = 0
axisOf D = 0
axisOf L = 1
axisOf R = 1
axisOf F = 2
axisOf B = 2

orient :: Piece -> Move -> Orientation
orient (Corner _ o) m
    | axisOf m == o = o
    | otherwise     = 3 - o - axisOf m 
orient (Edge _ _ o) m
    | axisOf m == 0 = o 
    | otherwise     = 1 - o

rotateCords :: Move -> Set Point -> Set Point
rotateCords m = Set.map moveBack . Set.map (rotateCord m) . Set.map moveCenter

moveCenter, moveBack :: Point -> Point
moveCenter (x,y,z) = (x-1,y-1,z-1)
moveBack   (x,y,z) = (x+1,y+1,z+1)

rotateCord :: Move -> Point-> Point
rotateCord U (x,y,z) = (-y, x, z)
rotateCord D (x,y,z) = ( y,-x, z)
rotateCord R (x,y,z) = ( x,-z, y)
rotateCord L (x,y,z) = ( x, z,-y)
rotateCord F (x,y,z) = ( z, y,-x)
rotateCord B (x,y,z) = (-z, y, x)
        
---- solvedCube == move (move (move (move solvedCube U) U) U) U

-- (Cube, [Move]) means a cube and the list move that it took to get there

--     to visit after   -> Set of Paths so far 
bfs :: [(Cube,[Move])] -> Set (Cube,[Move]) -> Set (Cube,[Move]) 
bfs []            v = v 
bfs ((cube,ts):c) v = if cube == solvedCube 
                      then Set.insert (cube,ts) v
                      else bfs (nubBy ( \x y -> fst x == fst y ) (c ++ dedup)) newV
    where
        dedup  = filter (\(c,p) -> Set.notMember c visitedCubes ) cubes 
        --cubes  = [ (move cube m ,ts++[m]) | m <- possibleMove cube ]
        cubes  = [ (nextCube , ts++[nextMove]) | (nextCube,nextMove) <- neighbours cube ]

        newV   = Set.insert (cube,ts) v
        visitedCubes = Set.map fst v

-------------------------------------------------------------------------------

scrambledCube = Set.fromList [-- x y z
        Corner (Set.fromList [(2,2,2)]) 0,

        Edge 0 (Set.fromList [(2,1,2)]) 0,
        Edge 2 (Set.fromList [(1,2,2)]) 1,
        Edge 1 (Set.fromList [(2,2,1)]) 1,

        Red    (Set.fromList [(1,0,2),(2,0,1),(2,0,2)]) 0,
        Red    (Set.fromList [(0,1,2),(0,2,2),(0,2,1)]) 0,
        Red    (Set.fromList [(1,2,0),(2,2,0),(2,1,0)]) 0,

        Orange (Set.fromList [(0,0,1),(0,0,2)]) 0,
        Orange (Set.fromList [(0,1,0),(0,2,0)]) 0,
        Orange (Set.fromList [(1,0,0),(2,0,0)]) 0,

        Blue   (Set.fromList [(0,0,0)]) 0 
       ]

solve start = fmap compact
            $ fmap snd 
            $ find (\(c,p) -> c == solvedCube ) 
            $ bfs [(start,[])] Set.empty


invert U = "U'"
invert F = "F'" 
invert B = "B'"
invert L = "L'"
invert R = "R'"
invert D = "D'"


compact (h1:h2:h3:t) 
    | h1 == h2 && h2 == h3 = invert h1 ++ compact t
    | otherwise = compact (h2:h3:t)
compact (h:t) = show h ++ compact t
compact [] = []


main = do
    print $ solve $ move solvedCube B
    print $ solve scrambledCube
    

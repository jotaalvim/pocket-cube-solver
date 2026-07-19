--             X    Y    Z
type Point = (Int, Int, Int)

type Orientation = Int

data Piece = Red    [Point] Orientation 
           | Orange [Point] Orientation 
           | Blue   [Point] Orientation 
           | Edge   [Point] Orientation
           | Corner [Point] Orientation
            -- | Center [Point] Orientation maybe redundant?
    deriving (Eq, Show)

data Move = U | D | B | F | L | R
    deriving (Eq, Show)


type Cube =  [Piece]

solvedCube = [-- x y z
        Corner [(2,2,2)] 0,
        Edge   [(2,1,2)] 0,
        Edge   [(1,2,2)] 0,
        Edge   [(2,1,2)] 0,

        Red    [(1,0,2),(2,0,1),(2,0,2)] 0,
        Red    [(0,1,2),(0,2,2),(0,2,1)] 0,
        Red    [(1,2,0),(2,2,0),(2,1,0)] 0,

        Orange [(0,0,1),(0,0,2)] 0,
        Orange [(0,1,0),(0,2,0)] 0,
        Orange [(1,0,0),(2,0,0)] 0,

        Blue [(0,0,0)] 0 
       ]

rotate :: a -> Move -> a
rotate = undefined



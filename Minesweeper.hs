module Minesweeper where

-- Used to index board (X, Y)
type Point = (Int, Int)

-- returns adjacent points around a sqare
-- starting at below the current point
adjacentPositions :: Point -> [Point]
adjacentPositions (x,y) = [(x,y-1), (x-1,y-1),
                           (x-1,y), (x-1,y+1),
                           (x,y+1), (x+1,y+1),
                           (x+1,y), (x+1,y-1)
                          ]

data Square = MineSquare
            | VisibleNumSquare { numSurrMines :: Int }
            | HiddenNumSquare { numSurrMines :: Int }
            | FlaggedSquare { flagged :: Square}

data Board = Board { width    :: Int
                   , height   :: Int
                   , numMines :: Int
                   , state    :: [[Square]] 
                   }

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard width height =
    Board width height 0 $ createGrid width height

createGrid :: Int -> Int -> [[Square]]
createGrid width height = replicate height . replicate width $ HiddenNumSquare 0
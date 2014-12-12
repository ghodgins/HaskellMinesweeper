{-#LANGUAGE LambdaCase, RecordWildCards #-}

module Minesweeper where

-- Used to index board (X, Y)
type Point = (Int, Int)

adjacentSquares :: Point -> Board -> [Point]
adjacentSquares point Board{..} = filter isValid . adjacentPoints $ point
    where
        isValid :: Point -> Bool
        isValid (x, y)
            | x < 0         = False
            | x >= width    = False
            | y < 0         = False
            | y >= height   = False
            | otherwise     = True

-- returns all adjacent points around a sqare
-- starting at below the current point
adjacentPoints :: Point -> [Point]
adjacentPoints (x,y) = [(x,y-1), (x-1,y-1),
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
module Minesweeper where

-- Used to index board
type Point = (Int, Int)

data Square = MineSquare
            | VisibleNumSquare { numSurrMines :: Int }
            | HiddenNumSquare { numSurrMines :: Int }
            | FlaggedSquare

data Board = Board { width    :: Int
                   , height   :: Int
                   , numMines :: Int
                   , state    :: [[Square]] 
                   }

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard width height =
    Board width height 0 $ createGrid width height

createGrid :: Int -> Int -> [[Square]]
createGrid width height = replicate height $ createRowSquares width

createRowSquares :: Int -> [Square]
createRowSquares length = replicate length $ HiddenNumSquare 0
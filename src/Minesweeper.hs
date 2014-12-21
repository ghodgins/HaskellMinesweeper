{-#LANGUAGE LambdaCase, RecordWildCards #-}

module Minesweeper where

import Square
import Types

data Board = Board { width    :: Int
                   , height   :: Int
                   , numMines :: Int
                   , board    :: [[Square]] 
                   }

instance Show Board where
    show (Board width height numMines board) =
        concatMap rowShow board
            where
                rowShow :: [Square] -> String
                rowShow row = (show row) ++ "\n"

createGameBoard :: Int -> Int -> [Point] -> Board
createGameBoard width height mines =
    Board width height (length mines) board
        where board = createGameGrid width height mines

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard width height =
    Board width height 0 $ createGrid width height

modifyBoard :: Board -> Point -> Square -> Board
modifyBoard Board{..} point square =
    case modifySquare board point square of
        board' -> Board width height numMines board'
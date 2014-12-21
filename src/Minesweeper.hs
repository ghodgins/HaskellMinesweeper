{-#LANGUAGE LambdaCase, RecordWildCards #-}

module Minesweeper where

import Square
import Types

data GameState = Won | Lost | Play

data Game = Game { state :: GameState
                 , numMines :: Int
                 , board    :: [[Square]] 
                 }

instance Show Game where
    show (Game state numMines board) =
        concatMap rowShow board
            where
                rowShow :: [Square] -> String
                rowShow row = (show row) ++ "\n"

createGame :: Int -> Int -> [Point] -> Game
createGame width height mines =
    Game Play (length mines) board
        where board = createGameGrid width height mines

modifyGame :: Game -> Point -> Square -> GameState -> Game
modifyGame Game{..} point square newState =
    case modifySquare board point square of
        board' -> (Game newState numMines board')

reveal :: Game -> Point -> Game
reveal game@Game{..} (x, y) = case board!!y!!x of
    (HiddenNumSquare val) -> modifyGame game (x, y) (VisibleNumSquare val) Play
    (HiddenMineSquare)    -> modifyGame game (x, y) (VisibleMineSquare) Lost
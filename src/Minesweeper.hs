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
    (HiddenNumSquare 0)    -> zeroFlood (modifyGame game (x, y) (VisibleNumSquare 0) Play) (x, y)
    (HiddenNumSquare val)  -> modifyGame game (x, y) (VisibleNumSquare val) Play
    (HiddenMineSquare)     -> modifyGame game (x, y) (VisibleMineSquare) Lost
    _                      -> game

flag :: Game -> Point -> Game
flag game@Game{..} (x, y) = case board!!y!!x of
    (FlaggedSquare square)       -> modifyGame game (x, y) square Play
    square@(HiddenNumSquare val) -> modifyGame game (x, y) (FlaggedSquare square) Play
    square@(HiddenMineSquare)    -> modifyGame game (x, y) (FlaggedSquare square) Play
    _                            -> game

zeroFlood :: Game -> Point -> Game
zeroFlood game@Game{..} point = revealIfZero game $ adjacentSquares point (length board) (length (board!!0))

revealIfZero :: Game -> [Point] -> Game
revealIfZero game [] = game
revealIfZero game@Game{..} ((x, y):xs) = case board!!y!!x of
    (HiddenNumSquare 0) -> revealIfZero (modifyGame game (x, y) (VisibleNumSquare 0) Play) xs
    _                   -> revealIfZero game xs

squareState :: Game -> Point -> Square
squareState Game{..} (x,y) = board!!y!!x

{-#LANGUAGE LambdaCase, RecordWildCards #-}

module Minesweeper where

import Square
import Types

import System.Random
import Data.List

data GameState = Won | Lost | Play deriving(Eq)

data Game = Game { state :: GameState 
                 , numMines :: Int
                 , board    :: [[Square]] 
                 }deriving(Eq)

instance Show Game where
    show (Game state numMines board) =
        concatMap rowShow board
            where
                rowShow :: [Square] -> String
                rowShow row = (show row) ++ "\n"

createGame :: Int -> Int -> Int -> StdGen -> Game
createGame width height numMines rng = Game Play numMines board
    where board = createGameGrid width height mines
          mines = generateMines width height numMines rng

modifyGame :: Game -> Point -> Square -> GameState -> Game
modifyGame Game{..} point square newState =
    case modifySquare board point square of
        board' -> (Game newState numMines board')

allPoints :: Game -> Int -> [Point] -> [Point]
allPoints game@Game{..} 0 acc = acc ++ [(0,0)]
allPoints game@Game{..} count acc = allPoints game (count-1) $  acc ++ [(count `mod` (length board), quot count (length board))]

reveal :: Game -> Point -> Game
reveal game@Game{..} (x, y) = case board!!y!!x of
    (HiddenNumSquare 0)    -> zeroFlood (modifyGame game (x, y) (VisibleNumSquare 0) (checkGameWin game)) (x, y)
    (HiddenNumSquare val)  -> modifyGame game (x, y) (VisibleNumSquare val) (checkGameWin game)
    (HiddenMineSquare)     -> modifyGame (revealAll game (allPoints game (((length board)*(length (board!!0)))-1) [] )) (x, y) (VisibleMineSquare) Lost
    _                      -> game

revealAll :: Game -> [Point] -> Game
revealAll game@Game{..} [] = game
revealAll game@Game{..} ((x, y):xs) = case board!!y!!x of
    (HiddenNumSquare 0)    -> revealAll (modifyGame game (x, y) (VisibleNumSquare 0) Lost) xs
    (HiddenNumSquare val)  -> revealAll (modifyGame game (x, y) (VisibleNumSquare val) Lost) xs
    (HiddenMineSquare)     -> revealAll (modifyGame game (x, y) (VisibleMineSquare) Lost) xs
    (FlaggedSquare square) -> revealAll (modifyGame game (x, y) square Lost) ((x,y):xs)
    _                      -> revealAll game xs

flag :: Game -> Point -> Game
flag game@Game{..} (x, y) = case board!!y!!x of
    (FlaggedSquare square)       -> modifyGame game (x, y) square Play
    square@(HiddenNumSquare val) -> modifyGame game (x, y) (FlaggedSquare square) Play
    square@(HiddenMineSquare)    -> modifyGame game (x, y) (FlaggedSquare square) (checkGameWin game)
    _                            -> game

-- Win: No mine squares on the board & num of flagged squares = number of mines
checkGameWin :: Game -> GameState
checkGameWin game@Game{..} = if checkForMines game && checkFlagsUsed game
                                 then Won
                                 else Play

checkForMines :: Game -> Bool
checkForMines Game{..} = all (\x' -> x' /= HiddenMineSquare) (concat board)

checkFlagsUsed :: Game -> Bool
checkFlagsUsed Game{..} = if countFlaggedMines board == numMines
                              then True
                              else False

countFlaggedMines :: [[Square]] -> Int
countFlaggedMines board = count (FlaggedSquare (HiddenMineSquare)) (concat board)

count :: Eq a => a -> [a] -> Int
count x = length . filter (\x' -> x' == x)

zeroFlood :: Game -> Point -> Game
zeroFlood game@Game{..} point = revealIfZero game $ adjacentSquares point (length board) (length (board!!0))

revealIfZero :: Game -> [Point] -> Game
revealIfZero game [] = game
revealIfZero game@Game{..} ((x, y):xs) = case board!!y!!x of
    (HiddenNumSquare 0)   -> revealIfZero (modifyGame game (x, y) (VisibleNumSquare 0) Play) (xs ++ (adjacentSquares (x, y) (length board) (length (board!!0))))
    (HiddenNumSquare val) -> revealIfZero (modifyGame game (x, y) (VisibleNumSquare val) Play) xs
    _                     -> revealIfZero game xs

squareState :: Game -> Point -> Square
squareState Game{..} (x,y) = board!!y!!x

generateMines :: Int -> Int -> Int -> StdGen -> [Point]
generateMines width height numMines rng = take numMines $ nub $ zip xs ys
    where xs = randomRs (0, width-1) (fst $ split rng)
          ys = randomRs (0, height-1) (snd $ split rng)

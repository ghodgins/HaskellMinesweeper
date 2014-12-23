{-#LANGUAGE LambdaCase, RecordWildCards #-}

module Minesweeper where

import Square
import Types

import System.Random
import Data.List

-- used to signal whether gamae has been won, lost or neither
data GameState = Won | Lost | Play

-- holds state of the game, number of mines in the board, and the board
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

-- creates the Game object, sets a random distribution of mines
createGame :: Int -> Int -> Int -> StdGen -> Game
createGame width height numMines rng = Game Play numMines board
    where board = createGameGrid width height mines
          mines = generateMines width height numMines rng

-- modifies a square on the board from the Game object
modifyGame :: Game -> Point -> Square -> GameState -> Game
modifyGame Game{..} point square newState =
    case modifySquare board point square of
        board' -> (Game newState numMines board')

-- generates a list of all points on the board
allPoints :: Game -> Int -> [Point] -> [Point]
allPoints game@Game{..} 0 acc = acc ++ [(0,0)]
allPoints game@Game{..} count acc = allPoints game (count-1) $  acc ++ [(count `mod` (length board), quot count (length board))]

-- reveals a square on the board, checks win/lose and zero floods
reveal :: Game -> Point -> Game
reveal game@Game{..} (x, y) = case board!!y!!x of
    (HiddenNumSquare 0)    -> zeroFlood (modifyGame game (x, y) (VisibleNumSquare 0) (checkGameWin game)) (x, y)
    (HiddenNumSquare val)  -> modifyGame game (x, y) (VisibleNumSquare val) (checkGameWin game)
    (HiddenMineSquare)     -> modifyGame (revealAll game (allPoints game (((length board)*(length (board!!0)))-1) [] )) (x, y) (VisibleMineSquare) Lost
    _                      -> game

-- reveals tiles on the board, used if user reveals a mine
revealAll :: Game -> [Point] -> Game
revealAll game@Game{..} [] = game
revealAll game@Game{..} ((x, y):xs) = case board!!y!!x of
    (HiddenNumSquare 0)    -> revealAll (modifyGame game (x, y) (VisibleNumSquare 0) Lost) xs
    (HiddenNumSquare val)  -> revealAll (modifyGame game (x, y) (VisibleNumSquare val) Lost) xs
    (HiddenMineSquare)     -> revealAll (modifyGame game (x, y) (VisibleMineSquare) Lost) xs
    (FlaggedSquare square) -> revealAll (modifyGame game (x, y) square Lost) ((x,y):xs)
    _                      -> revealAll game xs

-- flags/unflags a tile on the board
flag :: Game -> Point -> Game
flag game@Game{..} (x, y) = case board!!y!!x of
    (FlaggedSquare square)       -> modifyGame game (x, y) square Play
    square@(HiddenNumSquare val) -> modifyGame game (x, y) (FlaggedSquare square) Play
    square@(HiddenMineSquare)    -> modifyGame game (x, y) (FlaggedSquare square) (checkGameWin game)
    _                            -> game

-- Win: No mine squares on the board & num of flagged squares = number of mines
checkGameWin :: Game -> GameState
checkGameWin game@Game{..} = if checkMinesFlagged game && checkFlagsUsed game
                                 then Won
                                 else Play

-- returns true if there are no mines left unflagged
checkMinesFlagged :: Game -> Bool
checkMinesFlagged Game{..} = all (\x' -> x' /= HiddenMineSquare) (concat board)

-- checks if the number of flagged squares is equal to the number of mines
checkFlagsUsed :: Game -> Bool
checkFlagsUsed Game{..} = if countFlaggedMines board == numMines
                              then True
                              else False

-- counts the number of flagged mines on the board
countFlaggedMines :: [[Square]] -> Int
countFlaggedMines board = count (FlaggedSquare (HiddenMineSquare)) (concat board)

-- counts how many occurences of something are in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (\x' -> x' == x)

-- when a NumSquare with a value of 0 is revealed, it reveals adjacent NumSquares
zeroFlood :: Game -> Point -> Game
zeroFlood game@Game{..} point = revealIfZero game $ adjacentSquares point (length board) (length (board!!0))

-- helper function for zeroFlood
revealIfZero :: Game -> [Point] -> Game
revealIfZero game [] = game
revealIfZero game@Game{..} ((x, y):xs) = case board!!y!!x of
    (HiddenNumSquare 0)   -> revealIfZero (modifyGame game (x, y) (VisibleNumSquare 0) Play) (xs ++ (adjacentSquares (x, y) (length board) (length (board!!0))))
    (HiddenNumSquare val) -> revealIfZero (modifyGame game (x, y) (VisibleNumSquare val) Play) xs
    _                     -> revealIfZero game xs

-- returns a square on the board
squareState :: Game -> Point -> Square
squareState Game{..} (x,y) = board!!y!!x

-- generates a valid, random distribution of points for mines
generateMines :: Int -> Int -> Int -> StdGen -> [Point]
generateMines width height numMines rng = take numMines $ nub $ zip xs ys
    where xs = randomRs (0, width-1) (fst $ split rng)
          ys = randomRs (0, height-1) (snd $ split rng)
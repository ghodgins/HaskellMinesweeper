{-#LANGUAGE LambdaCase, RecordWildCards #-}

module Minesweeper where

-- Used to index board (X, Y)
type Point = (Int, Int)

adjacentSquares :: Point -> Int -> Int -> [Point]
adjacentSquares point width height = filter isValid . adjacentPoints $ point
    where
        isValid :: Point -> Bool
        isValid (x, y)
            | x < 0         = False
            | x >= width    = False
            | y < 0         = False
            | y >= height   = False
            | otherwise     = True

-- returns all adjacent points around a square
-- starting at below the current point
adjacentPoints :: Point -> [Point]
adjacentPoints (x,y) = [(x,y-1), (x-1,y-1),
                        (x-1,y), (x-1,y+1),
                        (x,y+1), (x+1,y+1),
                        (x+1,y), (x+1,y-1)
                       ]

data Square = VisibleMineSquare
            | HiddenMineSquare
            | VisibleNumSquare { numSurrMines :: Int }
            | HiddenNumSquare { numSurrMines :: Int }
            | FlaggedSquare { flagged :: Square}

instance Show Square where
    show VisibleMineSquare = "X"
    show HiddenMineSquare = " "
    show (VisibleNumSquare mines) = show mines
    show (HiddenNumSquare mines) = " "
    show (FlaggedSquare square) = "F"

data Board = Board { width    :: Int
                   , height   :: Int
                   , numMines :: Int
                   , state    :: [[Square]] 
                   }

instance Show Board where
    show (Board width height numMines state) =
        concatMap rowShow state
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

createGameGrid :: Int -> Int -> [Point] -> [[Square]]
createGameGrid width height minePoints =
    addMines minePoints $ createGrid width height

createGrid :: Int -> Int -> [[Square]]
createGrid width height = replicate height . replicate width $ HiddenNumSquare 0

addMines :: [Point] -> [[Square]] -> [[Square]]
addMines [] board = board
addMines (x:xs) board = addMines xs $ addMine board x

addMine :: [[Square]] -> Point -> [[Square]]
addMine board x = 
    incSurr x $ modifySquare board x (VisibleMineSquare)

-- increment squares surrounding newly placed mine
incSurr :: Point -> [[Square]] -> [[Square]]
incSurr x board = incSurr' (adjacentSquares x width height) board
    where height = length board
          width = length $ board!!0

incSurr' :: [Point] -> [[Square]] -> [[Square]]
incSurr' [] board = board
incSurr' (x:xs) board = incSurr' xs $ incrementNumSquare x board

incrementNumSquare :: Point -> [[Square]] -> [[Square]]
incrementNumSquare (x, y) board =
    case board!!x!!y of
        (HiddenNumSquare val) -> modifySquare board (x, y) (VisibleNumSquare (val+1))
        (VisibleNumSquare val) -> modifySquare board (x, y) (VisibleNumSquare (val+1))
        otherwise -> board

modifyBoard :: Board -> Point -> Square -> Board
modifyBoard Board{..} point square =
    case modifySquare state point square of
        board -> Board width height numMines board 

modifySquare :: [[Square]] -> Point -> Square -> [[Square]]
modifySquare board (row, column) newSquare =
    case splitAt column (board!!row) of
        (front, oldSpace:tail) -> restoreBoard board (front ++ newSquare : tail) row

restoreBoard :: [[Square]] -> [Square] -> Int -> [[Square]] 
restoreBoard board newRow splitRow =
    case splitAt splitRow board of
        (top, oldRow:bottom) -> top ++ newRow : bottom
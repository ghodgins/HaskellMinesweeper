module Square where

import Types

-- returns valid points surrounding a point on the board
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

-- returns all adjacent points starting at below the current point
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
    incSurr x $ modifySquare board x (HiddenMineSquare)

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
    case board!!y!!x of
        (HiddenNumSquare val) -> modifySquare board (x, y) (HiddenNumSquare (val+1))
        otherwise -> board

modifySquare :: [[Square]] -> Point -> Square -> [[Square]]
modifySquare board (x, y) newSquare =
    case splitAt x (board!!y) of
        (front, oldSpace:tail) -> restoreBoard board (front ++ newSquare : tail) y

restoreBoard :: [[Square]] -> [Square] -> Int -> [[Square]] 
restoreBoard board newRow splitRow =
    case splitAt splitRow board of
        (top, oldRow:bottom) -> top ++ newRow : bottom
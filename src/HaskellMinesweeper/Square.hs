module HaskellMinesweeper.Square where
import HaskellMinesweeper.Types

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
            | FlaggedSquare { flagged :: Square }
             deriving (Eq)

instance Show Square where
    show VisibleMineSquare = "X"
    show HiddenMineSquare = " "
    show (VisibleNumSquare mines) = show mines
    show (HiddenNumSquare mines) = " "
    show (FlaggedSquare square) = "F"

-- creates 2d list of squares (Hidden NumSquares and MineSquares)
createGameGrid :: Int -> Int -> [Point] -> [[Square]]
createGameGrid width height minePoints =
    addMines minePoints $ createGrid width height

-- creates 2d list of NumSquares to create the initial grid, no mines
createGrid :: Int -> Int -> [[Square]]
createGrid width height = replicate height . replicate width $ HiddenNumSquare 0

-- adds mines for each of the points in the list passed in, increments NumSquares around it
addMines :: [Point] -> [[Square]] -> [[Square]]
addMines [] board = board
addMines (x:xs) board = addMines xs $ addMine board x

-- adds a mine at the point specified and increments any NumSquares around it
addMine :: [[Square]] -> Point -> [[Square]]
addMine board x = 
    incSurr x $ modifySquare board x (HiddenMineSquare)

-- increment squares surrounding newly placed mine
incSurr :: Point -> [[Square]] -> [[Square]]
incSurr x board = incSurr' (adjacentSquares x width height) board
    where height = length board
          width = length $ board!!0

-- helper function for incSurr
incSurr' :: [Point] -> [[Square]] -> [[Square]]
incSurr' [] board = board
incSurr' (x:xs) board = incSurr' xs $ incrementNumSquare x board

-- helper function for incSurr'
incrementNumSquare :: Point -> [[Square]] -> [[Square]]
incrementNumSquare (x, y) board =
    case board!!y!!x of
        (HiddenNumSquare val) -> modifySquare board (x, y) (HiddenNumSquare (val+1))
        otherwise -> board

-- modifies a square on the board
modifySquare :: [[Square]] -> Point -> Square -> [[Square]]
modifySquare board (x, y) newSquare =
    case splitAt x (board!!y) of
        (front, oldSpace:tail) -> restoreBoard board (front ++ newSquare : tail) y

-- helper function for modifySquare
restoreBoard :: [[Square]] -> [Square] -> Int -> [[Square]] 
restoreBoard board newRow splitRow =
    case splitAt splitRow board of
        (top, oldRow:bottom) -> top ++ newRow : bottom
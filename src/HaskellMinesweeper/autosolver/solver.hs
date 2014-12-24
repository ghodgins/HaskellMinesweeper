{-#LANGUAGE RecordWildCards #-}

module Solver where

import Minesweeper
import Square
import Types
import Data.List

{-
1. traverse through every point on the map
2. if its a hidden numsquare, check that it is beside a visible numsquare.
3. if it is, add the point to a list
-}
hiddenFrontier :: Game -> [Point]
hiddenFrontier game@Game{..} = hiddenFrontier' (allPoints game (((length board)*(length (board!!0)))) []) board

hiddenFrontier' :: [Point] -> [[Square]] -> [Point]
hiddenFrontier' [] _ = []
hiddenFrontier' (x:xs) board = case isHidden (getSquare board x) && 
                                    isBesideVisible (getSquares board (adjacentSquares x (length board) (length (board!!0)))) of
    True  -> x : (hiddenFrontier' xs board)
    False -> hiddenFrontier' xs board

isHidden :: Square -> Bool
isHidden square = case square of
    (HiddenNumSquare _)  -> True
    _                    -> False

isBesideVisible :: [Square] -> Bool
isBesideVisible surrounding = any (\x -> isVisible x) surrounding

isVisible :: Square -> Bool
isVisible square = case square of
    (VisibleNumSquare _) -> True
    _                    -> False

-- returns the numbered squares next to unexplored fields
-- gets hiddenFrontier and finds complement set of it and all the points
visibleFrontier :: Game -> [Point]
visibleFrontier game@Game{..} = (allPoints game (((length board)*(length (board!!0)))) []) \\ hiddenFrontier game
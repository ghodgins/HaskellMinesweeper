import Minesweeper

{-
1. traverse through every point on the map
2. if its a hidden numsquare, check that it is beside a visible numsquare.
3. if it is, add the point to a list
-}
hiddenFrontier :: Game -> [Point]
hiddenFrontier game@Game{..} = hiddenFrontier' (allPoints game (((length board)*(length (board!!0))))) $ concat board

hiddenFrontier' :: [Point] -> [Square] -> [Point]
hiddenFrontier' points board = filter (is points board)

isHidden :: Square -> Bool
isHidden square = if square == (HiddenNumSquare _)
                      then True
                      else False

besideVisible :: Square -> [Square] -> Bool
besideVisible square surrounding = any (\x -> x == (VisibleNumSquare _)) surrounding

-- returns the numbered squares next to unexplored fields
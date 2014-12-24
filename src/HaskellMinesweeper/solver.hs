{-#LANGUAGE RecordWildCards #-}
module HaskellMinesweeper.Solver where
import qualified Data.Map

import HaskellMinesweeper.Minesweeper
import HaskellMinesweeper.Square
import HaskellMinesweeper.Types
import HaskellMinesweeper.ThirdParty as ThirdParty
import Data.List
import qualified Data.Set as Set

{-
1. traverse through every point on the map
2. if its a hidden numsquare, check that it is beside a visible numsquare.
3. if it is, add the point to a list
-}

performMove :: Game -> [(Point, [Point])] -> Game
performMove g p = flagSearch g m u $ ThirdParty.rref $ insertRows g m u p mx
    where
        m = indexMap p
        u = uniquePoints p
        mx = createMatrix (length u) (length m)

flagSearch :: Game -> [Point] -> [Point] -> [[Double]] -> Game
flagSearch g@Game{..} m u mx = placeFlags (revealFields g zeroPoints) flagPoints
    where
        (flags, zeros) = flagRowSearch mx [] []
        flagPoints = buildFlagPoints m u 0 [] flags
        zeroPoints = buildFlagPoints m u 0 [] zeros

placeFlags :: Game -> [Point] -> Game
placeFlags g@Game{..} [] = g
placeFlags g@Game{..} (x:xs) = case getSquare board x of
        (FlaggedSquare square) -> placeFlags g xs
        _                      -> placeFlags (flag g x) xs


revealFields :: Game -> [Point] -> Game
revealFields g@Game{..} [] = g
revealFields g@Game{..} (x:xs) = case getSquare board x of
        (FlaggedSquare square) -> revealFields (reveal (flag g x) x) xs -- Unflag & reveal
        _                      -> revealFields (reveal g x) xs

-- returns the numbered squares next to unexplored fields
-- gets hiddenFrontier and finds complement set of it and all the points
visibleFrontier :: Game -> [Point]
visibleFrontier game@Game{..} = visibleFrontier' (allPoints game (((length board)*(length (board!!0)))-1) []) board

visibleFrontier' :: [Point] -> [[Square]] -> [Point]
visibleFrontier' [] _ = []
visibleFrontier' (x:xs) board = case isVisible (getSquare board x) && 
                                    isBesideHidden (getSquares board (adjacentSquares x (length board) (length (board!!0)))) of
    True  -> x : (visibleFrontier' xs board)
    False -> visibleFrontier' xs board

isHidden :: Square -> Bool
isHidden square = case square of
    (HiddenNumSquare _)  -> True
    _                    -> False

isBesideHidden :: [Square] -> Bool
isBesideHidden surrounding = any (\x -> isHidden x) surrounding

isVisible :: Square -> Bool
isVisible square = case square of
    (VisibleNumSquare _) -> True
    _                    -> False

frontierEquations :: Game -> [Point] -> [(Point, [Point])]
frontierEquations game points = frontierEquations' game points []

frontierEquations' :: Game -> [Point] -> [(Point, [Point])] -> [(Point, [Point])]
frontierEquations' game@Game{..} [] ac = ac
frontierEquations' game@Game{..} (x:xs) ac = frontierEquations' game xs $ ac ++ [(x, pointEquation game x)]

-- Effectively given point's simultaneous equations can be generated from this
pointEquation :: Game -> Point -> [Point]
pointEquation game@Game{..} p = sort $ pointEquation' game (adjacentSquares p width height) []
    where height = length board
          width = length $ board!!0

pointEquation' :: Game -> [Point] -> [Point] -> [Point]
pointEquation' game@Game{..} [] a = a
pointEquation' game@Game{..} ((x,y):xs) a = case board!!y!!x of
    (VisibleNumSquare _)  -> pointEquation' game xs a
    _                    -> pointEquation' game xs $ [(x,y)] ++ a

buildFlagPoints :: [Point] -> [Point] -> Int -> [Point]  -> [Int] -> [Point]
buildFlagPoints m u i p [] = p
buildFlagPoints m u i p (x:xs) = case x of
    -1 -> buildFlagPoints m u (i + 1) p xs
    i  -> buildFlagPoints m u (i + 1) (p ++ [u!!i]) xs


-- flag row search returns matrix elements from which either
-- (a) The rightmost column of a reduced row echelon form matrix is 0
-- (b) The rightmoster column of a reduced row echelon form matrix is 1
-- Futhermore the all columns except for the t
flagRowSearch :: [[Double]] -> [Int] -> [Int] -> ([Int], [Int])
flagRowSearch [] f z = (f, z)
flagRowSearch (y:ys) flags zeros = case (Set.size ((Set.fromList y) Set.\\ (Set.fromList [0.0]))) of
    1 -> case (elemIndex ((filter (\x -> x /= 0) y)!!0) (init y)) of
        Just num -> case (last y) of
            0 -> flagRowSearch ys (flags ++ [-1]) (zeros ++ [num])
            1 -> flagRowSearch ys (flags ++ [num]) (zeros ++ [-1])
            _ -> next
        Nothing  -> next
    _ -> next
    where 
        next = flagRowSearch ys (flags ++ [-1]) (zeros ++ [-1])

--

indexMap :: [(Point, [Point])] -> [Point]
indexMap pts = map (\x -> fst x) pts

uniquePoints :: [(Point, [Point])] -> [Point]
uniquePoints pts = nub $ concat $ map (\x -> snd x) pts

createMatrix :: Int -> Int -> [[Double]]
createMatrix w h = replicate h . replicate (w+1) $ 0.0

insertRows :: Game -> [Point] -> [Point] -> [(Point, [Point])] -> [[Double]] -> [[Double]]
insertRows g@Game{..} m u [] mx = mx
insertRows g@Game{..} m u (x:xs) mx = insertRows g m u xs $ insertRow g m u x $ modifyMatrix mx (xcord, ycord) value
    where 
        (x', y') = fst x
        xcord = length u
        ycord = case (elemIndex (fst x) m) of
                Just v     -> v
                Nothing    -> 0
        value = case board!!y'!!x' of
            (VisibleNumSquare n)  -> (fromIntegral n)
            _                     -> 0 

insertRow :: Game -> [Point] -> [Point] -> (Point, [Point]) -> [[Double]]  -> [[Double]]
insertRow game@Game{..} map u (p, []) matrix = matrix
insertRow game@Game{..} map u (p, ((x,y):xs)) matrix = insertRow game map u (p, (xs)) $ modifyMatrix matrix (xcord, ycord) 1
    where 
        xcord = case elemIndex (x,y) u of
                Just v     -> v
                Nothing    -> 0
        ycord = case (elemIndex p map) of
                Just v     -> v
                Nothing    -> 0

-- modifies a square on the board
modifyMatrix :: [[Double]] -> (Int, Int) -> Double -> [[Double]]
modifyMatrix m (x, y) d =
    case splitAt x (m!!y) of
        (front, oldSpace:tail) -> restoreMatrix m (front ++ d : tail) y

-- helper function for modify Matrix
restoreMatrix :: [[Double]] -> [Double] -> Int -> [[Double]] 
restoreMatrix m newRow splitRow =
    case splitAt splitRow m of
        (top, oldRow:bottom) -> top ++ newRow : bottom
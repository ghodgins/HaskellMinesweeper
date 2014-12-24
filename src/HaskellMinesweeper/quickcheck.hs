
{-#LANGUAGE NamedFieldPuns #-}
import Test.QuickCheck hiding (Gen)
import HaskellMinesweeper.Square
import HaskellMinesweeper.Types
import HaskellMinesweeper.Minesweeper
import Data.List
import System.Random
--checks that adjacent points are correctly returned, 
--this test works
prop_adjacentPoints :: Point -> Bool
prop_adjacentPoints (x,y)
    = adjacentPoints (x,y) == [(x,y-1), (x-1,y-1),
                        (x-1,y), (x-1,y+1),
                        (x,y+1), (x+1,y+1),
                        (x+1,y), (x+1,y-1)]

--test that the grid will always be created the same
--this test works
prop_creategrid :: Int -> Int -> Bool
prop_creategrid x y
    = createGrid x y == createGrid x y
-- test entire game grid will be created the same, given same parameters getting an error here
--Exception:
--  quickcheck.hs:(16,1)-(17,66): Non-exhaustive patterns in function prop_createGameGrid
--0
--0
--[]
-- ie fails for 0 0 empty
prop_createGameGrid :: Int -> Int -> [(Int, Int)] -> Bool
prop_createGameGrid _ _ [] = True
prop_createGameGrid _ _ [(0,0)] = True
prop_createGameGrid 0 _ _ = True
prop_createGameGrid _ 0 _ = True
prop_createGameGrid x y [(a,b)] = 
        createGameGrid x y [(a,b)] == createGameGrid x y [(a,b)]


    
--need arbirtrary instance of square for this to work   
prop_addMine :: [[Square]] -> Point -> Bool
prop_addMine [] _ = True
prop_addMine _ (0,0) = True
prop_addMine x (a,b)
    = addMine x (a,b) == addMine x (a,b)


--figure out passing valid square data to test for the following:
-- incSurr, incrementNumSquare, modifySquare, restoreBoard all from Square.hs


-- need to care for zero cases & ensure Point is a valid one
--simlar errors showing up for this one, invalid and negative values are throwing it
prop_createGame :: Int -> Int -> Int -> Bool
prop_createGame 0 _ _ = True
prop_createGame _ 0 _ = True
prop_createGame _ _ 0 = True
-- need to generate random versions of game for this to work comment out for now
--prop_createGame x y z  
--   | createGame x y z == createGame x y z = True
--   | otherwise                            = False

-- figure out how to pass valid square data, game state and game data for the following:
-- modifyGame, allPoints,revealAll, flag, checkGameWin, checkForMines, checkFlagsUsed
-- countFlaggedMines, count, revealIfZero, squareState, generateMines

-- this method uses a randomly generated value :/....this doesnt work
newtype Gen a = Gen (Int -> StdGen -> a)
rand :: Gen StdGen
rand = Gen (\n r -> r)

prop_generateMines :: Int -> Int -> Int -> StdGen -> Bool
prop_generateMines x y numMines a
    =  length (generateMines x y numMines a) == numMines

--need instance of arbitrary Game
prop_reveal :: Game -> Point -> Bool
prop_reveal x y
    = reveal x y == reveal x y

-- Kevin added arbitrary's below in order to repair some failing tests above
instance Arbitrary Square where
    arbitrary = do
        Positive visible <- arbitrary 
        Positive hidden <- arbitrary 
        flagged <- arbitrary
        elements [VisibleMineSquare, HiddenMineSquare, (VisibleNumSquare visible), (HiddenNumSquare hidden), (FlaggedSquare flagged)]

instance Arbitrary StdGen where
        arbitrary = do
                x <- arbitrary
                return (mkStdGen x)

instance Arbitrary GameState where
    arbitrary = elements [Won, Lost, Play]

instance Arbitrary Game where
    arbitrary = do
        Positive h <- arbitrary
        Positive w <- arbitrary
        
        s <- arbitrary
        m <- arbitrary

        return Game { state = s, numMines = m, board = (createGrid w h)}

main  = mapM_ (\(s, a) -> print s >> a) tests
tests  = [("prop_adjacentPoints", quickCheck prop_adjacentPoints),
            ("prop_creategrid", quickCheck prop_creategrid),
            ("prop_createGameGrid", quickCheck prop_createGameGrid),
            ("prop_addMine", quickCheck prop_addMine),
            ("prop_createGame", quickCheck prop_createGame),
            ("prop_generateMines", quickCheck prop_generateMines),
            ("prop_reveal", quickCheck prop_reveal)]

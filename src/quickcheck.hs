
import Test.QuickCheck
import Square
import Types
import Minesweeper
import Data.List


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
prop_createGameGrid x y [(a,b)]
	= createGameGrid x y [(a,b)] == createGameGrid x y [(a,b)]

-- need to care for zero cases & ensure Point is a valid one
--simlar errors showing up for this one, invalid and negative values are throwing it
prop_createGame :: Int -> Int -> [(Int,Int)] -> Bool
prop_createGame 0 _ _ = True
prop_createGame _ 0 _ = True
prop_createGame _ _ [] = True
prop_createGame _ _ [(0,0)] = True

prop_createGame x y [(a,b)]  
	 | createGame x y [(a,b)] == createGame x y [(a,b)] = True
	 | otherwise 										= False
	

prop_addMine :: [[Square]] -> Point -> Bool
prop_addMine [[x]] (a,b)
	= addMine [[x]] (a,b) == addMine [[x]] (a,b)


main = quickCheck prop_addMine
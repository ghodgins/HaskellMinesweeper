
import Test.QuickCheck
import Square
import Types
import Minesweeper
import Data.List


--test that the grid will always be created the same
prop_creategrid :: Int -> Int -> Bool
prop_creategrid x y
	= createGrid x y == createGrid x y

rn = quickCheck prop_creategrid
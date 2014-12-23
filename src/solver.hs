import Data.Tensor.Vector
import Data.Tensor.LinearAlgebra hiding (Matrix)

main :: IO ()
main
  =  do 
  	let c = fromList [3.0,4.0,0.0,-1.0,4.0,5.0,6.0,2.0,1.0] :: Matrix Three Three Double
  	let k = rowEchelonForm c
  	print $ show c
  	print $ show k
  	print "lol"

--import qualified Data.Vector as V
--import Data.List
--import Minesweeper
--import Square

--data Matrix a = Matrix [[a]]
--              deriving (Eq)

---- Returns a list of X Y co-ordinates of the frontier points (hidden fields touched by numbered squares)
----frontier :: Game -> Int -> [Point] -> [Point]


---- 1. Use 'frontier' to get the X Y co-ordinates of the frontier
---- 2. Process each of the points returned
---- 2(a) Using 'adjacentSquares' in Square.hs, get the surronding points
---- 2(b) Count the number of hidden squares
---- 2(c) If the hidden squares equals the amount on the current square with have all mines.

--mop :: Num a => (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
--mop f = zipWith (zipWith f)

---- Swaps rows in positions i & i'
--mSwapRows :: Int -> Int -> [[a]] -> [[a]]
--mSwapRows i i' m = initial ++ (m !! b) : middle ++ (m !! a) : end 
--    where [a,b] = sort [i,i']
--          initial = take a m
--          middle  = take (b-a-1) (drop (a+1) m)
--          end     = drop (b+1) m

---- Performs operation f using a upon row i in matrix m, 
--mReplaceRow :: [a] -> Int -> [[a]] -> [[a]]
--mReplaceRow b i m = (initial ++ result : end)
--    where
--          initial = take i m
--          result  = b
--          end     = drop (i+1) m


--rref :: (Fractional a) => [[a]] -> [[a]]
--rref m = f m 0 [0 .. rows - 1]
--  where rows = length m
--        cols = length $ head m
--        f m _    []              = m
--        f m lead (r : rs)
--            | indices == Nothing = m
--            | otherwise          = f m' (lead' + 1) rs
--          where indices = find p l
--                p (col, row) = m !! row !! col /= 0
--                l = [(col, row) |
--                    col <- [lead .. cols - 1],
--                    row <- [r .. rows - 1]]

--                Just (lead', i) = indices
--                newRow = map (/ m !! i !! lead') $ m !! i

--                m' = zipWith g [0..] $
--                    replace r newRow $
--                    replace i (m !! r) m
--                g n row
--                    | n == r    = row
--                    | otherwise = zipWith h newRow row
--                  where h = subtract . (* row !! lead')

--replace :: Int -> a -> [a] -> [a]
--{- Replaces the element at the given index. -}
--replace n e l = a ++ e : b
--  where (a, _ : b) = splitAt n l

---- Performs operation f using a upon row i in matrix m, 
--mOpRows :: (a -> a -> a) -> [a] -> Int -> [[a]] -> [[a]]
--mOpRows f b i m = (initial ++ result : end)
--    where
--          initial = take i m
--          result  = zipWith f (m !! i) b
--          end     = drop (i+1) m

--instance Num a => Num (Matrix a)
--  where
--    (Matrix a) + (Matrix b) = Matrix $ mop (+) a b
--    (Matrix a) - (Matrix b) = Matrix $ mop (-) a b
--    (Matrix a) * (Matrix b) = Matrix $ mop (*) a b
--    negate                  = undefined
--    abs                     = undefined
--    signum                  = undefined
--    fromInteger             = undefined

--main :: IO ()
--main
--  =  do 
--  	let b = [[10, 20, 30, 40], [10, 20, 30, 40], [10, 20, 30, 40], [10, 20, 30, 40]]

--  	let k = mOpRows (+) [10, 20, 30, 80] 1 b 
--  	print $ show k
--  	print "lol"

--instance Show a => Show (Matrix a)
--  where
--    show (Matrix a) = intercalate "\n" $ map (intercalate " " . map show) a
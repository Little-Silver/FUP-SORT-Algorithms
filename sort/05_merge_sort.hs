import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 5000 0 100
--------------------------------------------------------------------------------------------
-- Merge sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

msort [] = []
msort xs = go [[x] | x <- xs]
    where
    go [a] = a
    go xs = go (pairs xs)
    pairs (a:b:t) = merge a b : pairs t
    pairs t = t
               
--------------------------------------------------------------------------------------------
-- Merge sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

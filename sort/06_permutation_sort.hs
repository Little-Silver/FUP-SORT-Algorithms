import Control.Exception
import Data.Time
import System.Random
import Data.List (permutations)

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 5000 0 100
--------------------------------------------------------------------------------------------
-- Permutation sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

psort :: Ord a => [a] -> [a]
psort = head . filter sorted . permutations
               
--------------------------------------------------------------------------------------------
-- Permutation sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

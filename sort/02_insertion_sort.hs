import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 500 0 100
--------------------------------------------------------------------------------------------
-- Insertion sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- ghci> isort list1               

--------------------------------------------------------------------------------------------
-- Insertion sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 500 0 100
--------------------------------------------------------------------------------------------
-- Bubble sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
bsort1 :: Ord a => [a] -> [a]
bsort1 [] = []
bsort1 s = case bsort1' s of
               t | t == s    -> t
                 | otherwise -> bsort1 t
  where bsort1' (x:y:xs) | x > y    = y:(bsort1' (x:xs))
                         | otherwise = x:(bsort1' (y:xs))
        bsort1' (a) = a          

-- ghci> bsort1 list1

--------------------------------------------------------------------------------------------
-- Bubble sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
bsort2 :: Ord a => [a] -> [a]
bsort2 list = bsort2' list (length list)

bsort2' :: Ord a => [a] -> Int -> [a]
bsort2' list 0 = list
bsort2' list n = bsort2' (bubblePass list) (n - 1)

bubblePass :: Ord a => [a] -> [a]
bubblePass [x] = [x]
bubblePass (x1 : x2 : xs)
  | x1 > x2 = x2 : bubblePass (x1 : xs)
  | otherwise = x1 : bubblePass (x2 : xs)

-- ghci> bsort1 list2
-- https://hackage.haskell.org/package/time
-- choco install ghc cabal
import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 5000 0 100

--Bubble sort -- Version 1
bsort :: Ord a => [a] -> [a]
bSort [] = []
bsort s = case bsort' s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where bsort' (x:y:xs) | x > y    = y:(bsort' (x:xs))
                         | otherwise = x:(bsort' (y:xs))
        bsort' (a) = a          
        --bsort' s = s

--Bubble sort -- Version 2
bubbleSort :: Ord a => [a] -> [a]
bubbleSort list = bubbleSort' list (length list)

bubbleSort' :: Ord a => [a] -> Int -> [a]
bubbleSort' list 0 = list
bubbleSort' list n = bubbleSort' (bubblePass list) (n - 1)

bubblePass :: Ord a => [a] -> [a]
bubblePass [x] = [x]
bubblePass (x1 : x2 : xs)
  | x1 > x2 = x2 : bubblePass (x1 : xs)
  | otherwise = x1 : bubblePass (x2 : xs)

--ghci> bsort1 list1
--[1,2,3,6,10]


-- Naive time measurement
main = do
    start <- getCurrentTime
    evaluate (bsort list1)
    end <- getCurrentTime
    print ("bsort: ")
    print (diffUTCTime end start)
    start2 <- getCurrentTime
    evaluate (bubbleSort list1)
    end2 <- getCurrentTime
    print ("bubbleSort: ")
    print (diffUTCTime end2 start2)

-- ghci> main
-- "bsort: "
-- 5.4741434s
-- "bubbleSort: "
-- 3.090575s

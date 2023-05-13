import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 5000 0 100
--------------------------------------------------------------------------------------------
-- Quick sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) = qsort1 [a | a <- xs, a < x]
                      ++ [x] ++
               qsort1 [b | b <- xs, b >= x]
               
--------------------------------------------------------------------------------------------
-- Quick sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
qsort2 :: Ord a => [a] -> [a]
qsort2 xs = qsort2' xs []

qsort2' :: Ord a => [a] -> [a] -> [a]
qsort2' [] acc = acc
qsort2' (x:xs) acc = qsort2' smallerOrEqual (x : qsort2' larger acc)
  where
    smallerOrEqual = filter (<= x) xs
    larger = filter (> x) xs

--------------------------------------------------------------------------------------------
-- Quick sort -- Version 3 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
qsort3 :: Ord a => [a] -> [a]
qsort3 (x:xs) = qsort3 (filter (<= x) xs) 
                     ++ [x] ++ 
                qsort3 (filter (> x) xs)
qsort3 x = x

run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)


main = do
    run qsort1 list1 "Simple Implementation 1"
    run qsort2 list1 "Tail Recursive"
    run qsort3 list1 "Simple Implementation 2"
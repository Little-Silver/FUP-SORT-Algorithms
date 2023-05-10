import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 5000 0 100
--------------------------------------------------------------------------------------------
-- Quick sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x]
                      ++ [x] ++
               qsort [b | b <- xs, b >= x]
               
--------------------------------------------------------------------------------------------
-- Quick sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)


main = do
    run qsort list1 "qsort"
    run qsort list1 "qsort"
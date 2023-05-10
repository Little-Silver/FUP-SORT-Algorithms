import Control.Exception
import Data.Time
import System.Random
import Data.List (permutations)

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 20 0 10
list2 = reverse [0..100]
list3 = [0..100]
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

run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)


main = do
    run psort list1 "psort"
    run psort list1 "psort"
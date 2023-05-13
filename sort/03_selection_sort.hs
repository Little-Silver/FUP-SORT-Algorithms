import Control.Exception
import Data.Time
import System.Random
import Data.List (delete, minimum)

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 5000 0 1000
--------------------------------------------------------------------------------------------
-- Selection sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
ssort1 :: Ord t => [t] -> [t]
ssort1 [] = []
ssort1 xs = let { x = minimum xs } 
           in  x : ssort1 (delete x xs)

-- ghci> ssort1 list1               

--------------------------------------------------------------------------------------------
-- Selection sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
ssort2 :: Ord t => [t] -> [t]
ssort2 xs = ssort2' [] xs

ssort2' :: Ord t => [t] -> [t] -> [t]
ssort2' xs [] = reverse xs
ssort2' xs ys = let {y = minimum ys } 
                in ssort2' (y:xs) (delete y ys)


-- ghci> bsort1 list2
run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)


main = do
    run ssort1 list1 "ssort1"
    run ssort2 list1 "ssort2"
import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 900 0 1000
--------------------------------------------------------------------------------------------
-- Bubble sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
bsort1 :: Ord a => [a] -> [a]
bsort1 [] = []
bsort1 s = case bsort1' s of
               t | t == s    -> t
                 | otherwise -> bsort1 t
  where 
        bsort1' :: Ord a => [a] -> [a]
        bsort1' (x:y:xs) | x > y    = y:(bsort1' (x:xs))
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

--------------------------------------------------------------------------------------------
-- Bubble sort -- Version 3 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

bsort3 :: Ord a => [a] -> [a]   
bsort3 list = bsort3' True [] list
    where 
        bsort3' :: Ord a => Bool -> [a] -> [a] -> [a]   
        bsort3' False s [x] = (++) s [x]
        bsort3' True s [x]  = bsort3' False [] ((++) s [x])
        bsort3' flag s (x:y:xs) | x > y     = bsort3' True ((++) s [y]) (x:xs)
                                | otherwise  = bsort3' flag ((++) s [x]) (y:xs)

--------------------------------------------------------------------------------------------
-- Bubble sort -- Version 4 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
bsort4 :: Ord a => [a] -> [a]   
bsort4 list = bsort3' False [] (reverse list)
    where 
        bsort3' :: Ord a => Bool -> [a] -> [a] -> [a]   
        bsort3' False s [x] = reverse (x:s)
        bsort3' True s [x]  = bsort3' False [] (reverse (x:s))
        bsort3' flag s (x:y:xs) | x > y      = bsort3' True (y:s) (x:xs)
                                | otherwise  = bsort3' flag (x:s) (y:xs)

--------------------------------------------------------------------------------------------
-- Bubble sort -- Version 5 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

bsort5 :: Ord a => [a] -> [a]
bsort5 [] = []
bsort5 s = case bsort5' [] s of
               t | t == s    -> t
                 | otherwise -> bsort5 t
  where 
        bsort5' :: Ord a => [a] -> [a] -> [a]
        bsort5' s (x:y:xs) | x > y    = bsort5' (y:s) (x:xs)
                         | otherwise = bsort5' (x:s) (y:xs)
        bsort5' s (a) = reverse(a ++ s)


-- ghci> bsort1 list2
run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)

main = do
    run bsort1 list1 "bsort1 - Thomas"
    run bsort2 list1 "bsort2 - ChatGPT"
    run bsort3 list1  "bsort3 - Pascal: Tail Recursive 1"
    run bsort4 list1  "bsort4 - Pascal: Tail Recursive 2"
    run bsort5 list1  "bsort5 - Pascal: Tail Recursive 3"
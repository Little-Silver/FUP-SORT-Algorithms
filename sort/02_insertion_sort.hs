import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 5000 0 100
--------------------------------------------------------------------------------------------
-- Insertion sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
isort1 :: Ord a => [a] -> [a]
isort1 [] = []
isort1 (x:xs) = insert x (isort1 xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y     = x:y:ys
                | otherwise = y:(insert x ys)

-- ghci> isort list1               

--------------------------------------------------------------------------------------------
-- Insertion sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
isort2 :: Ord a => [a] -> [a]
isort2 xs = isort2' xs []

isort2' :: Ord a => [a] -> [a] -> [a]
isort2' [] acc = acc
isort2' (x:xs) acc = isort2' xs (insert2 x acc)

insert2 :: Ord a => a -> [a] -> [a]
insert2 x [] = [x]
insert2 x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert2 x ys


-- ghci> bsort1 list2
run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)


main = do
    run isort1 list1 "isort"
    run isort2 list1 "isort"
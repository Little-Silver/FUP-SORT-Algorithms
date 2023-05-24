import Data.List (delete, minimum)
import Data.List (permutations)
import Control.Exception
import Data.Time
import System.Random


-- stack install random -> Powershell
-- import System.Random -> ghci

list_test = [13,10,1,2,6,3,5,9,4,11]

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

--list1 = reverse [0..1000] --randomIntList 900 0 1000
list1 = randomIntList 10 0 1000
list2 = randomIntList 50 0 1000
list3 = randomIntList 500 0 1000
list4 = randomIntList 1000 0 1000
list5 = randomIntList 10000 0 10000
list6 = randomIntList 100000 0 100000
list500 = reverse [0..500]
list1000 = reverse [0..1000]
list10000 = reverse [0..10000]
list100 = reverse [0..100000]


--Merge sort
---------------------------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
--ghci> merge [1,2,3] [4,5] -> [1,2,3,4,5]

pairs :: Ord a => [[a]] -> [[a]]
pairs (a:b:t) = merge a b : pairs t
pairs t = t

go :: Ord a => [[a]] -> [a]
go [a] = a
go xs = go (pairs xs)

--Bottom-up version:
msort [] = []
msort xs = go (map (\x -> [x]) xs)
    
--Top-down version:
my_split :: [a] -> ([a], [a])
my_split [] = ([], [])
my_split [x] = ([x], [])
my_split (x:y:xs) = (x:xs1, y:xs2)
  where
    (xs1, xs2) = my_split xs

msort2 :: Ord a => [a] -> [a]
msort2 [] = []
msort2 [x] = [x]
msort2 xs = merge (msort2 firstHalf) (msort2 secondHalf)
  where
    (firstHalf, secondHalf) = my_split xs


{-
merge:
The merge function merges two lists into a single list.

pairs:
It takes a list of lists as input and merges adjacent pairs of sublists until only one list remains. 

go:
The go function takes a list of lists xs as input and recursively merges adjacent pairs of lists until one single sorted list remains.
-}

run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)

main = do
    run msort list1000 "msort_bottom-up_reverse1000 ------------"
    run msort2 list1000 "msort_top-down_reverse1000 ------------"
    run msort list6 "msort_bottom-up_random100'000 ------------"
    run msort2 list6 "msort_top-down_random100'000 ------------"
    run msort list100 "msort_bottom-up_reverse100'000 ------------"
    run msort2 list100 "msort_top-down_reverse100'000 ------------"


{-
ghci> main
"msort_bottom-up_reverse1000 ------------: "
0.0010783s
"msort_top-down_reverse1000 ------------: "
0.0059846s
"msort_bottom-up_random100'000 ------------: "
0.1097047s
"msort_top-down_random100'000 ------------: "
0.5455754s
"msort_bottom-up_reverse100'000 ------------: "
0.1017276s
"msort_top-down_reverse100'000 ------------: "
0.5276258s
-}



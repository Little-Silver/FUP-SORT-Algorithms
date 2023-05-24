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
list2 = randomIntList 11 0 1000

list5 = reverse [0..5]
list10 = reverse [0..10]
list12 = reverse [0..12]
list11 = reverse [0..11]
list15 = reverse [0..15]
list20 = reverse [0..20]
list30 = reverse [0..30]
list50 = reverse [0..50]

--Permutation Sort, also known as bogosort
sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _ = True

psort :: Ord a => [a] -> [a]
psort = head . filter sorted . permutations
--ghci> psort list1
--[1,2,3,6,10]

my_remove :: Eq a => a -> [a] -> [a]
my_remove _ [] = []
my_remove y (x:xs)
  | y == x = xs
  | otherwise = x : my_remove y xs

my_perms :: Eq a => [a] -> [[a]]
my_perms [] = [[]]
my_perms xs = foldr (\x acc -> acc ++ (map (x:) (my_perms (my_remove x xs)))) [] (my_nub xs)
--ghci> my_perms [1,1,2]
--[[2,1,1],[1,2,1],[1,1,2]]

fhead :: [a] -> Maybe a
fhead []    = Nothing
fhead (x:_) = Just x

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter predicate (x:xs)
  | predicate x = x : my_filter predicate xs
  | otherwise = my_filter predicate xs

my_nub :: Eq a => [a] -> [a]
my_nub [] = []
my_nub (x:xs) = x : my_nub (filter (/= x) xs)

unwrapMaybe :: Maybe a -> a
unwrapMaybe (Just x) = x
unwrapMaybe Nothing  = error "Value is missing"

--my_psort :: Ord a => [a] -> Maybe [a]
--my_psort = fhead . my_filter sorted . my_perms
my_psort :: Ord a => [a] -> [a]
my_psort = unwrapMaybe . fhead . my_filter sorted . my_perms
--ghci> my_psort list2
--Just [1,2,3,4,5,6,9,10,11,13]

{-
The 'permutations' function returns the list of all permutations of the argument.
ghci> permutations [1,2,3]
[[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]

The 'head' function returns the first item of a list.
head :: [a] -> a
head (x:_) = x
head []    = error "empty list"
ghci> head[1,2,3]
1   

fhead :: [a] -> Maybe a
fhead []    = Nothing
fhead (x:_) = Just x

filter :: (a -> Bool) -> [a] -> [a]
filter (your_predicate) your_list
ghci> filter odd list2
[13,1,3,5,9,11]

Delete Function -> removes the first occurrence of the specified element from its list argument
The otherwise guard should always be last, it’s like the
default case in a C-style switch statement.
Eq a => a -> [a] -> [a]
delete _ [] = []
delete y (x:xs)
  | y == x = xs
  | otherwise = x : delete y xs

ghci> delete 2 [1,2,3,2,1]
[1,3,2,1]

my_perms
(map (x:) (perms (remove x xs))) generates a list of permutations where x is the first element.
It does this by mapping (x:) over the permutations generated from the remaining elements of xs
(with x removed using the remove function).
acc ++  appends the list of permutations to the accumulator acc.

Time Complexity: 
Worst Case : O(∞) (since this algorithm has no upper bound)
Average Case: O(n*n!)
Best Case : O(n)(when array given is already sorted)


xs = [1, 2, 3, 4, 5]
element = xs !! 2
->  element at index 2 -> 3
-}


randomPermutation :: [a] -> IO [a]
randomPermutation xs = do
  gen <- newStdGen
  return $ shuffle' xs (length xs) gen
  where
    shuffle' :: [a] -> Int -> StdGen -> [a]
    shuffle' [] _ _ = []
    shuffle' ys n g
      | n <= 0 = ys
      | otherwise =
        let (index, newGen) = randomR (0, n - 1) g
            (front, x:back) = splitAt index ys
        in x : shuffle' (front ++ back) (n - 1) newGen

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:rest) = x <= y && isSorted (y:rest)

shuffleUntilSorted :: Ord a => [a] -> IO [a]
shuffleUntilSorted xs = randomPermutation xs >>= \shuffled ->
  if isSorted shuffled
    then pure shuffled
    else shuffleUntilSorted xs



run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)

main = do
    --run psort list5 "psort5 ------------"
    --run my_psort list5 "my_psort5 ------------"
    run psort list10 "psort10 -----------"
    run my_psort list10 "my_psort10 -----------"
    run shuffleUntilSorted list10 "shuffleUntilSorted10 -----------"
    run psort list11 "psort11 -----------"
    run shuffleUntilSorted list11 "shuffleUntilSorted11 -----------"
    run my_psort list11 "my_psort11 -----------"
    --run my_psort list12 "my_psort12 -----------"
    run my_psort list15 "my_psort15 -----------"
    --run psort list1 "psort_random10 -----------"
    run my_psort list1 "my_psort_random10 -----------"
    run shuffleUntilSorted list1 "shuffleUntilSorted_random10 -----------"
    --run psort list2 "psort_random11 -----------"
    --run my_psort list2 "my_psort_random11 -----------"



{-
ghci> main
"psort5 ------------: "
0.0008083s
"psort10 -----------: "
3.3903901s
"psort11 -----------: "
36.6387702s
"psort12 -----------: "
457.1566776s
-}

{-
ghci> main
"psort5 ------------: "
0s
"my_psort5 ------------: "
0s
"psort10 -----------: "
3.3611361s
"my_psort10 -----------: "
0s
"psort11 -----------: "
37.0321704s
"my_psort11 -----------: "
0s
"my_psort12 -----------: "
0s
"my_psort15 -----------: "
0s
"psort_random10 -----------: "
2.3427692s
"my_psort_random10 -----------: "
0.1705778s
"psort_random11 -----------: "
28.4778172s
"my_psort_random11 -----------: "
27.761904s
-}

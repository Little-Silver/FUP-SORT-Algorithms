import Data.List (delete, minimum)
import Data.List (permutations)
import Control.Exception
import Data.Time
import System.Random

-- stack install random -> Powershell
-- import System.Random -> ghci

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list_test = [13,10,1,2,6,3,5,9,4,11]
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
---------------------------------------------------------------------

-- Checks if list is sorted:
sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _ = True
--sorted [1,2,3,6,10] -> True
--sorted [1,2,11,6,10] -> False

psort :: Ord a => [a] -> [a]
psort = head . filter sorted . permutations
--ghci> psort list1 -> [1,2,3,6,10]

{-
The 'permutations' function returns the list of all permutations of the argument.
ghci> permutations [1,2,3] -> [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
ghci> permutations [1,1,2] -> [[1,1,2],[1,1,2],[2,1,1],[1,2,1],[2,1,1],[1,2,1]]

The 'head' function returns the first item of a list.
ghci> head [13,10,1,2,6,3,5,9,4,11] -> 13

ghci> filter odd [13,10,1,2] -> [13,1]
-}

-- Removes the first occurrence of an element from a list.
my_remove :: Eq a => a -> [a] -> [a]
my_remove _ [] = []
my_remove y (x:xs)
  | y == x = xs
  | otherwise = x : my_remove y xs

-- Gives list of permutations as list without duplicates
my_perms :: Eq a => [a] -> [[a]]
my_perms [] = [[]]
my_perms xs = foldr (\x acc -> acc ++ (map (x:) (my_perms (my_remove x xs)))) [] (my_nub xs)
--ghci> my_perms [1,1,2] -> [[2,1,1],[1,2,1],[1,1,2]]

fhead :: [a] -> Maybe a
fhead []    = Nothing
fhead (x:_) = Just x

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter predicate (x:xs)
  | predicate x = x : my_filter predicate xs
  | otherwise = my_filter predicate xs

-- Gives list without duplicates
my_nub :: Eq a => [a] -> [a]
my_nub [] = []
my_nub (x:xs) = x : my_nub (filter (/= x) xs)

unwrapMaybe :: Maybe a -> a
unwrapMaybe (Just x) = x
unwrapMaybe Nothing  = error "Value is missing"

my_psort :: Ord a => [a] -> [a]
my_psort = unwrapMaybe . fhead . my_filter sorted . my_perms
--ghci> my_psort list2 -> [95,95,208,466,500,535,560,651,663,783,913]

{-
my_perms
(map (x:) (perms (remove x xs))) generates a list of permutations where x is the first element.
It does this by mapping (x:) over the permutations generated from the remaining elements of xs.
acc ++  appends the list of permutations to the accumulator acc.
-}

-- Gives one random permutation
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
--ghci> randomPermutation [1,1,2] -> [2,1,1]

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:rest) = x <= y && isSorted (y:rest)

-- Basic bogosort implementation
shuffleUntilSorted :: Ord a => [a] -> IO [a]
shuffleUntilSorted xs = randomPermutation xs >>= \shuffled ->
  if isSorted shuffled
    then pure shuffled
    else shuffleUntilSorted xs


{-
Time Complexity: 
Worst Case : O(∞) (since this algorithm has no upper bound)
Average Case: O(n*n!)
Best Case : O(n)(when array given is already sorted)
-}


run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)

main = do
    run psort list10 "psort_reverse10 -----------"
    run my_psort list10 "my_psort_reverse10 -----------"
    run my_psort list1 "my_psort_random10 -----------"
    run shuffleUntilSorted list10 "shuffleUntilSorted_reverse10 -----------"
    run shuffleUntilSorted list1 "shuffleUntilSorted_random10 -----------"
    run psort list11 "psort_reverse11 -----------"
    run my_psort list11 "my_psort_reverse11 -----------"
    run shuffleUntilSorted list11 "shuffleUntilSorted_reverse11 -----------"
    run my_psort list15 "my_psort_reverse15 -----------"



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

import Data.List (delete, minimum)
import Data.List (permutations)

list1 = [10,1,2,6,3]


--Bubble sort
bsort :: Ord a => [a] -> [a]
bSort [] = []
bsort s = case bsort' s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where bsort' (x:y:xs) | x > y    = y:(bsort' (x:xs))
                         | otherwise = x:(bsort' (y:xs))
        bsort' (a) = a          
        --bsort' s = s
--ghci> bsort1 list1
--[1,2,3,6,10]


--Quicksort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x]
                      ++ [x] ++
               qsort [b | b <- xs, b >= x]
--ghci> quicksort list1
--[1,2,3,6,10]


--Insertion Sort
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
--ghci> isort list1
--[1,2,3,6,10]


--Selection sort
ssort :: Ord t => [t] -> [t]
ssort [] = []
ssort xs = let { x = minimum xs } 
           in  x : ssort (delete x xs)
--ghci> ssort list1
--[1,2,3,6,10]


--Merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

msort [] = []
msort xs = go [[x] | x <- xs]
    where
    go [a] = a
    go xs = go (pairs xs)
    pairs (a:b:t) = merge a b : pairs t
    pairs t = t
--ghci> msort list1
--[1,2,3,6,10]


--Permutation Sort
sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

psort :: Ord a => [a] -> [a]
psort = head . filter sorted . permutations
--ghci> psort list1
--[1,2,3,6,10]


--Binary Tree Sort
-- Binary Search Tree Datatype (Belongs to BTsort along with add, addList and inorder)
data Tree a = Node a (Tree a) (Tree a) | Empty 

-- add an Element to the Tree
add :: Ord a => a -> Tree a -> Tree a
add n Empty        = Node n Empty Empty
add n (Node v l r) = if n < v then Node v (add n l) r else Node v l (add n r)

-- add a List of Elements to a Tree
addList :: Ord a => Tree a -> [a] -> Tree a
addList = foldr add

-- make inorder list of a tree
inorder :: Tree a -> [a]
inorder Empty        = []
inorder (Node v l r) = inorder l ++ (v : inorder r)

-- Sort a list by adding all it's elements to a binary search Tree and return it's inorder list
treeSort :: Ord a => [a] -> [a]
treeSort = inorder . addList Empty 
--ghci> treeSort list1
--[1,2,3,6,10]


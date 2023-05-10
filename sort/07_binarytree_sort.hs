import Control.Exception
import Data.Time
import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 500 0 100
--------------------------------------------------------------------------------------------
-- Binary Tree sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

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


               
--------------------------------------------------------------------------------------------
-- Binary Tree -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------


run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)


main = do
    run treeSort list1 "treeSort"
    run treeSort list1 "treeSort"
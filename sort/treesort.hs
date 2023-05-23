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


{-
The Tree type has two constructors:
Node, which represents a node in the tree with a value and two child trees, and Empty,
which represents an empty tree.

add:
If the tree is empty, a new Node is created with the given element as the value, and two empty child trees.
If the tree is a Node, the function compares the given element (n) with the value of the current node (v).
n < v -> recursively calls add on the left child tree (l)
else -> recursively calls add on the right child tree (r)
The implementation ensures that the resulting tree maintains the binary search tree property,
where all elements in the left subtree are less than the value of the current node,
and all elements in the right subtree are greater than or equal to the value of the current node.

addList:
By applying foldr add to the list of elements, the addList function effectively adds each element
from the list to the binary search tree. The result is a new binary search tree that contains
all the elements from the input list.

inorder:
By recursively traversing the left subtree, processing the current node,
and then traversing the right subtree,
the inorder function effectively generates a list of elements in the binary search tree in ascending order.

treeSort:
Composing inorder . addList Empty
addList Empty takes a list of elements and adds them to an initially empty binary search tree. 
The inorder function generates an inorder traversal of the tree, which returns a list of elements in ascending order.
The list is initially being added to an empty tree, and each element is inserted into the tree using the add function.
-}

run f l fname = do
    start <- getCurrentTime
    evaluate (f l)
    end <- getCurrentTime
    print ((++) fname  ": ")
    print (diffUTCTime end start)

main = do
    run treeSort list3 "treeSort_random500 ------------"
    run treeSort list500 "treeSort_reverse500 ------------"
    run treeSort list4 "treeSort_random1000 ------------"
    run treeSort list1000 "treeSort_reverse1000 ------------"
    run treeSort list5 "treeSort_random10'000 ------------"
    run treeSort list10000 "treeSort_reverse10'000 ------------"
    run treeSort list6 "treeSort_random100'000 ------------"
    run treeSort list100 "treeSort_reverse100'000 ------------"

{-

-}



import Control.Exception
import Data.Time
import System.Random
import Data.List (delete, minimum)

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

list1 = randomIntList 500 0 100
--------------------------------------------------------------------------------------------
-- Selection sort -- Version 1 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------
ssort :: Ord t => [t] -> [t]
ssort [] = []
ssort xs = let { x = minimum xs } 
           in  x : ssort (delete x xs)

-- ghci> ssort list1               

--------------------------------------------------------------------------------------------
-- Selection sort -- Version 2 ----------------------------------------------------------------
--------------------------------------------------------------------------------------------

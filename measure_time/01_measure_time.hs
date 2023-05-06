-- https://hackage.haskell.org/package/time
-- choco install ghc cabal
import Control.Exception
import Data.Time


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


-- Naive time measurement
main = do
    start <- getCurrentTime
    evaluate (bsort list1)
    end <- getCurrentTime
    print (diffUTCTime end start)

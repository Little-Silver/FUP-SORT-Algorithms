import System.Random

randomIntList :: Int -> Int -> Int -> [Int]
randomIntList n minVal maxVal = take n $ randomRs (minVal, maxVal) (mkStdGen 42)

main :: IO ()
main = do
  let randomList = randomIntList 50 0 1000
  print randomList

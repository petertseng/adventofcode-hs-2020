import AdventOfCode (readInputFile)

import Data.List (foldl')

sumBetween :: Int -> Int -> Int
sumBetween l r = pair * numPairs + (numHalfPairs * pair `div` 2)
  where size = r - l + 1
        (numPairs, numHalfPairs) = size `divMod` 2
        pair = r + l

seat :: String -> Int
seat = foldl' (\a c -> a * 2 + bit c) 0

bit :: Char -> Int
bit 'R' = 1
bit 'B' = 1
bit 'L' = 0
bit 'F' = 0
bit c = error ("invalid char " ++ [c])

main :: IO ()
main = do
  s <- readInputFile
  let seats = map seat (lines s)
      maxId = maximum seats
  print maxId
  print (sumBetween (minimum seats) maxId - sum seats)

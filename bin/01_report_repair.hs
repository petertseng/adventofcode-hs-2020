import AdventOfCode (readInputFile)

import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (tails)

-- could use find to get only the first solution,
-- but am interested to see how long it takes to find them all.
productOfSum :: IntMap Int -> [(Int, Int, Int)] -> [Int]
productOfSum nums = map (\(x, y, _) -> x * y) . filter (\(x, _, i) -> IntMap.findWithDefault 0 x nums > i)

main :: IO ()
main = do
  s <- readInputFile
  let nums = map read (lines s)
      indexedNums = zip nums [0..]
      numsSet = IntMap.fromList indexedNums
      showProducts = traverse_ print . productOfSum numsSet
  showProducts [(2020 - x, x, i) | (x, i) <- indexedNums]
  showProducts [(2020 - x - y, x * y, j) | (x, _):xs <- tails indexedNums, (y, j) <- xs]

import AdventOfCode (readInputFile)

import qualified Data.IntMap as IntMap
import Data.List (foldl', sort)

waysByMap, waysByFold :: Num a => [Int] -> a
waysByMap adapters = snd (IntMap.findMax ways')
  where ways' = IntMap.fromListWith (+) ((0, 1) : [
          (x, IntMap.findWithDefault 0 (x - y) ways')
          | x <- adapters, y <- [1, 2, 3]
          ])

waysByFold = trd . foldl' ways' (0, 0, 1)
  where trd (_, _, x) = x
        ways' (a, b, c) diff = case diff of
          1 -> (b, c, a + b + c)
          2 -> (c, 0, b + c)
          3 -> (0, 0, c)
          _ -> error ("bad diff " ++ show diff)

-- one wants adapters and one wants diffs so...
ways, _ways :: Num a => [Int] -> [Int] -> a
ways _ = waysByFold
_ways adapters _ = waysByMap adapters

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let adapters = sort (map read (lines s))
      diffs = zipWith (-) adapters (0 : adapters)
  print (count (== 1) diffs * (count (== 3) diffs + 1))
  print (ways adapters diffs :: Int)

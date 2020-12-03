import AdventOfCode (readInputFile)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (catMaybes)

type Row = (IntSet, Int)

trees, treesByEvery, treesByMod, _trees :: [Row] -> (Int, Int) -> Int

treesByEvery rows (dx, dy) = countIndexed treeAtI (every dy rows)
  where treeAtI i r = treeAt r (i * dx)

treesByMod rows (dx, dy) = countIndexed treeAtY rows
  where treeAtY y r = y `mod` dy == 0 && treeAt r (y * dx `div` dy)

-- Seems there's not much difference in perf between the two impls,
-- so I'll arbitrarily pick one.
_trees = treesByMod
trees = treesByEvery

treeAt :: Row -> Int -> Bool
treeAt (treeXs, width) x = IntSet.member (x `mod` width) treeXs

every :: Int -> [a] -> [a]
-- the code would still be correct without this 1 case,
-- but maybe having it causes `every 1 xs` to do less useless work?
every 1 xs = xs
every _ [] = []
every n (x:xs) = x : every n (drop (n - 1) xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

-- Is this even worth extracting...
countIndexed :: (Int -> a -> Bool) -> [a] -> Int
countIndexed f = count id . zipWith f [0..]

row :: String -> Row
row s = (IntSet.fromList treeXs, length s)
  -- this could be elemIndices '#' s,
  -- but I guess I'll keep this for the validation that it's only '#' or '.'
  where treeXs = catMaybes (zipWith tree s [0..])
        tree '#' i = Just i
        tree '.' _ = Nothing
        tree c i = error ("bad char " ++ [c] ++ " at " ++ show i)

main :: IO ()
main = do
  s <- readInputFile
  let rows = map row (lines s)
      treesProduct slopes = product (map (trees rows) slopes)
  print (treesProduct [(3, 1)])
  print (treesProduct [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])

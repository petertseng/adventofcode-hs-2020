import AdventOfCode (readInputFile, splitOn, splitOnOne)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow ((***), first, second)
import Control.Monad (forM_)
import Data.Function (on)
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe, maybe)
import Data.List (dropWhileEnd, elemIndex, find, findIndex, foldl', group, groupBy, inits, intercalate, intersperse, maximumBy, minimumBy, partition, permutations, sort, sortOn, sortBy, subsequences, tails, transpose)
import Debug.Trace
import Text.Printf (printf)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  print (count (== "42") (lines s))

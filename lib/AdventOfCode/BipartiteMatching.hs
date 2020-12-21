module AdventOfCode.BipartiteMatching (
  match,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

-- Left if we're uncertain, Right if we're certain
match :: (Ord k, Ord v) => Map k (Set v) -> Map k (Either (Set v) v)
--match m = untilEq (nakedSingle . hiddenSingle) (Map.map Left m)
match m = untilEq nakedSingle (Map.map Left m)

--nakedSingle, hiddenSingle :: (Ord k, Ord a) => Map k (Either (Set a) a) -> Map k (Either (Set a) a)
nakedSingle :: (Ord k, Ord a) => Map k (Either (Set a) a) -> Map k (Either (Set a) a)
nakedSingle ps = foldl' confirm ps (mapMaybe singleLeft (Map.assocs ps))
  where confirm p (l, r) = Map.insert l (Right r) (Map.map (mapLeft (Set.delete r)) p)
        singleLeft (l, Left rs) | Set.size rs == 1 = Just (l, Set.findMin rs)
        singleLeft (_, _) = Nothing

-- Never needed this so I guess I won't bother...
--hiddenSingle ps = ps

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

untilEq :: Eq a => (a -> a) -> a -> a
untilEq f = fst . until2 (==) f

until2 :: (a -> a -> Bool) -> (a -> a) -> a -> (a, a)
until2 p f x = let x' = f x in if p x x' then (x, x') else until2 p f x'

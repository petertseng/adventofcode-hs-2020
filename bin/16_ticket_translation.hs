import AdventOfCode (readInputFile)
import AdventOfCode.BipartiteMatching (match)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Range = (Int, Int)
newtype Field = Field String deriving (Eq, Ord)

-- Invalid: Left, invalid values
-- Valid: Right, the values unaltered
validate :: [Range] -> [Int] -> Either [Int] [Int]
validate ranges ticket = case filter (\v -> not (any (`inRange` v) ranges)) ticket of
  [] -> Right ticket
  invalids -> Left invalids

matchFields :: [[Int]] -> [(Field, [Range])] -> Map Int (Either (Set Field) Field)
matchFields tickets fieldsAndRanges = match (initialMatches tickets fieldsAndRanges)

initialMatches :: [[Int]] -> [(Field, [Range])] -> Map Int (Set Field)
initialMatches tickets fieldsAndRanges = Map.fromListWith Set.intersection (concatMap (indexFieldsPairs fieldsAndRanges) tickets)

-- For one ticket, pairs of (index, fields that could match it)
indexFieldsPairs :: [(Field, [Range])] -> [Int] -> [(Int, Set Field)]
indexFieldsPairs fieldsAndRanges ticket = zip [0..] (map (Set.fromList . fieldsValidFor) ticket)
  where fieldsValidFor v = map fst (filter (\(_, ranges) -> any (\(l, r) -> l <= v && v <= r) ranges) fieldsAndRanges)

merge :: [Range] -> [Range]
merge = mergeSorted . sort

mergeSorted :: [Range] -> [Range]
mergeSorted [] = []
mergeSorted ((min1, max1):(min2, max2):xs) | succ max1 >= min2 = mergeSorted ((min1, max max1 max2) : xs)
mergeSorted (x:xs) = x : mergeSorted xs

inRange :: Range -> Int -> Bool
inRange (l, r) v = l <= v && v <= r

parseFields :: String -> (Field, [Range])
parseFields s = (Field field, ranges)
  where (field, rangesStr) = splitOnOne ':' s
        ranges = mapMaybe parseRange (words rangesStr)

parseRange :: String -> Maybe Range
parseRange s | not (any isDigit s) = Nothing
parseRange s = let (l, r) = splitOnOne '-' s in Just (read l, read r)

parseTicket :: String -> [Int]
parseTicket = map read . splitOn ','

main :: IO ()
main = do
  s <- readInputFile
  let (fieldsStr, mineStr, nearbyStr) = case splitOn "" (lines s) of
        [fs, ["your ticket:", ms], "nearby tickets:":ns] -> (fs, ms, ns)
        [_, _, _] -> error "three sections malformed"
        _ -> error "not three sections"
      fields = map parseFields fieldsStr
      -- not necessary to merge the ranges, but wanted to write the code to see how it works out.
      validRangesForAny = merge (concatMap snd fields)
      (invalids, valids) = partitionEithers (map (validate validRangesForAny . parseTicket) nearbyStr)
  print (sum (concat invalids))

  let mine = parseTicket mineStr
      confirmedFields = matchFields valids fields
      departs = zipWith depart mine (Map.assocs confirmedFields)
      depart v (_, Right (Field f)) | "departure" `isPrefixOf` f = v
      depart _ _ = 1
  print (product departs)

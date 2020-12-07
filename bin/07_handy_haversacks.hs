{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow ((***))
import Data.Either (rights)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map

newtype Bag = Bag String deriving (Eq, Ord)

recursivelyContain :: Map Bag [Bag] -> Bag -> [(Int, Bag)]
recursivelyContain contain bag =
  rights (bfs (flip (Map.findWithDefault []) contain) (/= bag) bag)

recursivelyContainedBy :: Map Bag [(Int, Bag)] -> Bag -> Int
recursivelyContainedBy containedBy =
  sum . map (\(qnt, contain) -> qnt * (1 + recursivelyContainedBy containedBy contain)) . (containedBy Map.!)

bagRule :: String -> (Bag, [(Int, Bag)])
bagRule s = (Bag containing, contained)
  where (containingWords, containedWords) = splitOnOne "contain" (words s)
        containing = unwords (takeWhile (/= "bags") containingWords)
        containedStrs = splitOn ',' (unwords containedWords)
        contained = if containedStrs == ["no other bags."] then [] else map bagAndQuantity containedStrs

bagAndQuantity :: String -> (Int, Bag)
bagAndQuantity s = case words s of
  (quantity : colourWords) -> (read quantity, Bag colour)
    where colour = unwords (takeWhile (not . ("bag" `isPrefixOf`)) colourWords)
  _ -> error ("no words " ++ s)

invert :: [(a, [b])] -> [(b, a)]
invert = concatMap invert'
  where invert' (a, bs) = map (, a) bs

myBag :: Bag
myBag = Bag "shiny gold"

main :: IO ()
main = do
  s <- readInputFile
  let containedBy = map bagRule (lines s)
      -- invert gives [((Int, Bag), Bag)], now need [(Bag, [Bag])] for fromListWith.
      contain = Map.fromListWith (++) (map (snd *** (: [])) (invert containedBy))
  print (length (recursivelyContain contain myBag))
  print (recursivelyContainedBy (Map.fromList containedBy) myBag)

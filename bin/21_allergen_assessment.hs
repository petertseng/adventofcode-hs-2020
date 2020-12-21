import AdventOfCode (readInputFile)
import AdventOfCode.BipartiteMatching (match)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Ingredient = Ingredient String deriving (Eq, Ord)
newtype Allergen = Allergen String deriving (Eq, Ord)

partitionAllergens :: [([Ingredient], [Allergen])] -> (Map Allergen (Set Ingredient), Set Ingredient)
partitionAllergens foods = (allergenCouldBe, notAllergens)
  where ingredients = Set.fromList (concatMap fst foods)
        allergenCouldBe = Map.fromListWith Set.intersection kvs
        kvs = [(a, Set.fromList is) | (is, as) <- foods, a <- as]
        notAllergens = foldl' Set.difference ingredients (Map.elems allergenCouldBe)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

parse :: String -> ([Ingredient], [Allergen])
parse s = (is, as)
  where (l, r) = splitOnOne '(' s
        is = map Ingredient (words l)
        as = map (Allergen . dropWhile (== ' ')) (splitOn ',' (drop 9 (init r)))

main :: IO ()
main = do
  s <- readInputFile
  let foods = map parse (lines s)
      (allergenCouldBe, notAllergens) = partitionAllergens foods
  print (sum (map (\(is, _) -> count (`Set.member` notAllergens) is) foods))

  let rightIngred (_, Right (Ingredient i)) = i
      rightIngred (Allergen a, Left _) = error ("couldn't deduce " ++ a)
  putStrLn (intercalate "," (map rightIngred (Map.assocs (match allergenCouldBe))))

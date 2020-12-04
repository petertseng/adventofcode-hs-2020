import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.Char (isDigit, isHexDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readMaybe)

type Passport = Map String String

valid1 :: Passport -> Bool
valid1 p = Map.keysSet validPassport `Set.isSubsetOf` Map.keysSet p

valid2 :: Passport -> Bool
valid2 p = valid1 p && and (Map.elems (Map.intersectionWith ($) validPassport p))

validPassport :: Map String (String -> Bool)
validPassport = Map.fromList [
    ("byr", numBetween 1920 2002)
  , ("iyr", numBetween 2010 2020)
  , ("eyr", numBetween 2020 2030)
  , ("hgt", validHeight)
  , ("hcl", \s -> length s == 7 && head s == '#' && all isHexDigit (tail s))
  , ("ecl", flip Set.member validEyes)
  , ("pid", \s -> length s == 9 && all isDigit s)
  ]

numBetween :: Int -> Int -> String -> Bool
numBetween l r = maybe False (\yr -> l <= yr && yr <= r) . readMaybe

validHeight :: String -> Bool
validHeight (a:b:c:"cm") = numBetween 150 193 [a, b, c]
validHeight (a:b:"in") = numBetween 59 76 [a, b]
validHeight _ = False

validEyes :: Set String
validEyes = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

passport :: String -> Passport
passport = Map.fromList . map (splitOnOne ':') . words

main :: IO ()
main = do
  s <- readInputFile
  let passportGroups = splitOn "" (lines s)
      passportStrings = map unwords passportGroups
      passports = map passport passportStrings
  print (count valid1 passports)
  print (count valid2 passports)

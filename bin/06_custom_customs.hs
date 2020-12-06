import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.Bits ((.&.), (.|.), bit, popCount)
import Data.Char (ord)
import Data.List (foldl')

bits :: String -> Int
bits = sum . map (bit . subtract (ord 'a') . ord)

aToZ :: Int
aToZ = bits ['a' .. 'z']

main :: IO ()
main = do
  s <- readInputFile
  let groups = splitOn "" (lines s)
      groupSets = map (map bits) groups
      -- this is probably some monoid thing
      count plus zero = sum (map (popCount . foldl' plus zero) groupSets)
  print (count (.|.) 0)
  print (count (.&.) aToZ)

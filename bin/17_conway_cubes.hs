import AdventOfCode (readInputFile)

import Control.Monad (replicateM)
import Data.Bits (countLeadingZeros, finiteBitSize, shiftL)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')

type Pos2 = (Int, Int)

step :: [Int] -> [Int] -> [Int]
step neigh actives = IntMap.keys (IntMap.filter (\n -> 5 <= n && n <= 7) neighAndSelf)
  where neighAndSelf = IntMap.fromListWith (+) (concatMap oneAndTwos actives)
        oneAndTwos pos = (pos, 1 :: Int) : [(pos + dpos, 2) | dpos <- neigh]

compress :: Int -> Int -> Int -> Int -> Int -> [Int] -> Int
compress offset yBits wzBits x y = foldl' shift (x' + y')
  where x' = (x + offset) `shiftL` yBits
        y' = y + offset
        shift acc c = (acc `shiftL` wzBits) + c

neighsInD :: Int -> Int -> Int -> [Int]
neighsInD dim yBits wzBits = [compress 0 yBits wzBits dx dy rest | dx:dy:rest <- tail (replicateM dim [0, -1, 1])]

bitWidth :: Int -> Int
bitWidth x = finiteBitSize x - countLeadingZeros x

enumGrid :: [[a]] -> [(Pos2, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let rows = lines s
      height = length rows
      rounds = 6
      yBits = bitWidth (height + rounds * 2 + 1)
      wzBits = bitWidth (rounds * 2 + 1)
      active2 = map fst (filter ((== '#') . snd) (enumGrid rows))
      activeInD dim = map (\(y, x) -> compress rounds yBits wzBits x y (replicate (dim - 2) 0)) active2
      f dim = length (iterate (step (neighsInD dim yBits wzBits)) (activeInD dim) !! rounds)
  print (f 3)
  print (f 4)

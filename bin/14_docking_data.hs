import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Data.Bits ((.|.), (.&.), complement, popCount, shiftL)
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (elemIndices, foldl')

-- Mask zeroes ones xs
-- Mem addr val
data Inst = Mask Int Int Int | Mem Int Int deriving (Show)

run :: [Inst] -> IntMap Int
run = fst3 . foldl' run' (IntMap.empty, 0, 0)
  where run' (mem, _, _) (Mask zs os _)  = (mem, zs, os)
        run' (mem, zero, one) (Mem addr val) = (IntMap.insert addr (mask zero one val) mem, zero, one)
        mask z o val = (val .|. o) .&. complement z

run2 :: [Inst] -> IntMap Int
run2 = fst3 . foldl' run' (IntMap.empty, 0, 0)
  where run' (mem, _, _) (Mask _ os xs) = (mem, os, xs)
        run' (mem, one, xs) (Mem addr val) = (IntMap.union newWrites mem, one, xs)
          where newWrites = IntMap.fromDistinctAscList [(addr', val) | addr' <- mask one xs addr]
        mask ones xs addr = map (addr' .|.) (subBits xs)
          where addr' = (addr .&. complement xs) .|. ones

subBits :: Int -> [Int]
subBits x = 0 : takeWhile (/= 0) (drop 1 (iterate nextPattern 0))
  where nextPattern n = (n + complement x + 1) .&. x

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

parseInst :: String -> Inst
parseInst ('m':'a':'s':'k':s) = parseMask (last (words s))
parseInst ('m':'e':'m':s) = parseMem s
parseInst s = error ("bad inst " ++ s)

parseMask :: String -> Inst
parseMask s = Mask (fromBits zs) (fromBits os) (fromBits xs)
  where rev = reverse s
        zs = elemIndices '0' rev
        os = elemIndices '1' rev
        xs = elemIndices 'X' rev

fromBits :: [Int] -> Int
fromBits = sum . map (1 `shiftL`)

parseMem :: String -> Inst
parseMem s = Mem addr val
  where (l, r) = splitOnOne '=' s
        addr = read (filter isDigit l)
        val = read (filter isDigit r)

tooManyXs :: Inst -> Bool
tooManyXs (Mask _ _ xs) = popCount xs >= 10
tooManyXs (Mem _ _) = False

main :: IO ()
main = do
  s <- readInputFile
  let insts = map parseInst (lines s)
  print (sum (IntMap.elems (run insts)))

  if any tooManyXs insts
    then putStrLn "too many floating bits"
    else print (sum (IntMap.elems (run2 insts)))

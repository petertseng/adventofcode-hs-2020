import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.List (foldl', minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

type Bus = (Int, Int)

departMatchingId :: Int -> [Int] -> (Int, Int)
departMatchingId t0 = minimumBy (comparing fst) . map wait
  where wait busId = ((-t0) `mod` busId, busId)

departMatchingOffset :: [Bus] -> Int
departMatchingOffset = snd . foldl' bus (1, 0)
  where bus (step, t) (busId, busOffset) =
          let (x, g) = gcdExtPos step busId
              initialDiff = -busOffset - t :: Int
          in if initialDiff `mod` g /= 0 then error ("bad " ++ show (initialDiff, g, step, busId))
             else (lcm step busId, t + ((step * (((initialDiff `mod` busId) * x) `mod` busId)) `div` g))

gcdExtPos :: Integral a => a -> a -> (a, a)
gcdExtPos a n = let (x, _, g) = gcdExt a n in (mkPos x, g)
  where mkPos x | x < 0 = x + n
        mkPos x = x

-- https://rosettacode.org/wiki/Modular_inverse#Haskell
-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

parse :: String -> [Bus]
parse str = mapMaybe bus (zip [0..] (splitOn ',' str))
  where bus (_, "x") = Nothing
        bus (offset, busId) = Just (read busId, offset)

main :: IO ()
main = do
  s <- readInputFile
  let (t0, buses) = case lines s of
        [t, b] -> (read t, parse b)
        _ -> error "not two lines"
  let (wait, bus) = departMatchingId t0 (map fst buses)
  print (wait * bus)
  print (departMatchingOffset buses)

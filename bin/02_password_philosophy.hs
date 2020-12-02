import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

type Entry = (Int, Int, Char, String)

good1 :: Entry -> Bool
good1 (l, r, c, pw) = l <= n && n <= r
  where n = count (== c) pw

good2 :: Entry -> Bool
good2 (l, r, c, pw) = check l /= check r
  where check i = pw !! (i - 1) == c

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

policy :: String -> Entry
policy s = (l, r, c, pw)
  where nums : (c:_) : pw : _ = words s
        (l, r) = tmap read (splitOnOne '-' nums)
        tmap f (a, b) = (f a, f b)

main :: IO ()
main = do
  s <- readInputFile
  let policies = map policy (lines s)
  print (count good1 policies)
  print (count good2 policies)

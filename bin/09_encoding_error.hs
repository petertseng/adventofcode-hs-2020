import AdventOfCode (readInputFileAndFlags)

import Data.Array.Unboxed ((!), UArray, listArray)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)

findNotSum :: Window -> Int -> Either Int Window
findNotSum win x | winfull win && winnone (\y -> y * 2 /= x && winhas (x - y) win) win = Left x
findNotSum win x = Right (push x win)

findSum :: UArray Int Int -> Int -> (Int, Int)
findSum cumulativeSum = findSum' 0 0
  where findSum' l r 0 = (l, r)
        findSum' l r diff | diff > 0 =
          -- window too small; expand window's right side
          findSum' l (r + 1) (diff + cumulativeSum ! r - cumulativeSum ! (r + 1))
        findSum' l r diff =
          -- window too large; shrink window's left side
          findSum' (l + 1) r (diff - cumulativeSum ! l + cumulativeSum ! (l + 1))

data Queue a = Queue [a] [a]

qpush :: a -> Queue a -> Queue a
qpush e (Queue inb out) = Queue (e:inb) out

qpop :: Queue a -> (a, Queue a)
qpop (Queue [] []) = error "never happens in day 9"
qpop (Queue inb []) = qpop (Queue [] (reverse inb))
qpop (Queue inb (x:xs)) = (x, Queue inb xs)

type Window = (Queue Int, IntMap Int, Int)

window :: Int -> Window
window cap = (Queue [] [], IntMap.empty, cap)

winany :: (Int -> Bool) -> Window -> Bool
winany f (Queue a b, _, _) = any f a || any f b

winnone :: (Int -> Bool) -> Window -> Bool
winnone f w = not (winany f w)

winhas :: Int -> Window -> Bool
winhas x (_, freq, _) = IntMap.findWithDefault 0 x freq > 0

winfull :: Window -> Bool
winfull (_, _, cap) = cap == 0

push :: Int -> Window -> Window
push x (q, freq, cap) =
  let q' = qpush x q
      freq' = IntMap.insertWith (+) x 1 freq
  in case cap of
    0 -> let (popped, q'') = qpop q' in (q'', IntMap.adjust pred popped freq', 0)
    _ -> (q', freq', cap - 1)

-- Left to stop iteration (mnemonic left to leave), right to continue
foldEither :: (a -> b -> Either c a) -> a -> [b] -> Maybe c
foldEither _ _ [] = Nothing
foldEither f acc (x:xs) = case f acc x of
  Left res -> Just res
  Right acc' -> foldEither f acc' xs

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let nums = map read (lines s)
      winSize = maybe 25 read (lookup 'n' flags)
      target = fromJust (foldEither findNotSum (window winSize) nums)
  print target

  let cumulativeSum = listArray (0, length nums) (0 : scanl1 (+) nums)
      (l, r) = findSum cumulativeSum target
      sumToTarget = take (r - l) (drop l nums)
  print (minimum sumToTarget + maximum sumToTarget)

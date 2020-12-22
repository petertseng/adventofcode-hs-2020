import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Data.Array.Unboxed ((!), UArray, accumArray)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Prelude hiding (round)

combat :: [Int] -> [Int] -> [Int]
combat xs [] = xs
combat [] ys = ys
combat (x:xs) (y:ys) | x > y = combat (xs ++ [x, y]) ys
combat (x:xs) (y:ys) | y > x = combat xs (ys ++ [y, x])
combat (_:_) (_:_) = error "no ties"

notKnownToLoop :: UArray Int Bool
notKnownToLoop = accumArray (||) False (1, 50) [(n, True) | n <- [1, 2, 3, 4, 6, 8, 12, 24, 32, 38, 40, 42, 44, 46, 48, 49, 50]]

recursiveCombat :: Bool -> Int -> Seq Int -> Seq Int -> Either (Seq Int) (Seq Int)
recursiveCombat False maxCard s1 _ | maximum s1 == maxCard = Left undefined
recursiveCombat False _ s1 s2 | notKnownToLoop ! (Seq.length s1 + Seq.length s2) = case compare (maximum s1) (maximum s2) of
  GT -> Left undefined
  LT -> Right undefined
  EQ -> error "no ties"
recursiveCombat _ maxCard s1 s2 = game Set.empty s1 s2
  where game seen xs ys = case (Seq.viewl xs, Seq.viewl ys) of
          (Seq.EmptyL, _) -> Right ys
          (_, Seq.EmptyL) -> Left xs
          -- only check cache if high card about to be drawn
          (x Seq.:< xs', y Seq.:< ys') | x == maxCard || y == maxCard ->
            let key = cacheKey xs ys
            in if key `Set.member` seen then Left xs else round x xs' y ys' (Set.insert key seen)
          -- no check cache, just do round
          (x Seq.:< xs', y Seq.:< ys') -> round x xs' y ys' seen
        cacheKey a b = toList a ++ 0 : toList b
        round x xs y ys seen =
          let winner = if x <= Seq.length xs && y <= Seq.length ys
                then let sub1 = Seq.take x xs
                         sub2 = Seq.take y ys
                  in mapEither (const ()) (recursiveCombat False (max (maximum sub1) (maximum sub2)) sub1 sub2)
                else case compare x y of
                  GT -> Left ()
                  LT -> Right ()
                  EQ -> error "no ties"
          in case winner of
            Left ()  -> game seen (xs Seq.|> x Seq.|> y) ys
            Right () -> game seen xs (ys Seq.|> y Seq.|> x)

mapEither :: (a -> b) -> Either a a -> Either b b
mapEither f (Left l) = Left (f l)
mapEither f (Right r) = Right (f r)

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

main :: IO ()
main = do
  s <- readInputFile
  let (deck1s, deck2s) = splitOnOne "" (lines s)
      deck1 = map read (tail deck1s)
      deck2 = map read (tail deck2s)
  print (score (combat deck1 deck2))
  let maxCard = maximum (deck1 ++ deck2)
      windeck = either id id (recursiveCombat True maxCard (Seq.fromList deck1) (Seq.fromList deck2))
  print (score (toList windeck))

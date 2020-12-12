import AdventOfCode (readInputFile)

import Control.Arrow (first, second)
import Data.List (foldl')

type Coord = (Int, Int)
type CoordSelect = (Coord -> Coord) -> (Coord, Coord) -> (Coord, Coord)

move :: CoordSelect -> CoordSelect -> (Char, Int) -> (Coord, Coord) -> (Coord, Coord)
move movable _ ('N', mag) ship = movable (first (subtract mag)) ship
move movable _ ('S', mag) ship = movable (first (+ mag)) ship
move movable _ ('E', mag) ship = movable (second (+ mag)) ship
move movable _ ('W', mag) ship = movable (second (subtract mag)) ship
move _ turnable ('L', mag) ship = turnable (\t -> iterate left t !! quadrants mag) ship
move _ turnable ('R', mag) ship = turnable (\t -> iterate right t !! quadrants mag) ship
move _ _ ('F', mag) ((sy, sx), t@(ty, tx)) = ((sy + ty * mag, sx + tx * mag), t)
move _ _ (c, _) _ = error (c : " isn't a move")

quadrants :: Int -> Int
quadrants x = case x `divMod` 90 of
  (q, 0) -> q
  (_, _) -> error ("bad rotate " ++ show x)

left :: (Int, Int) -> (Int, Int)
left (y, x) = (-x, y)

right :: (Int, Int) -> (Int, Int)
right (y, x) = (x, -y)

l1 :: (Int, Int) -> Int
l1 (y, x) = abs y + abs x

parse :: String -> (Char, Int)
parse (x:xs) = (x, read xs)
parse [] = error "bad"

main :: IO ()
main = do
  s <- readInputFile
  let dirs = map parse (lines s)
      ship f turnable = fst (foldl' f ((0, 0), turnable) dirs)

      move1 :: (Coord, Coord) -> (Char, Int) -> (Coord, Coord)
      move1 = flip (move first second)
      ship1 = ship move1 (0, 1)
  print (l1 ship1)

  let move2 :: (Coord, Coord) -> (Char, Int) -> (Coord, Coord)
      move2 = flip (move second second)
      ship2 = ship move2 (-1, 10)
  print (l1 ship2)

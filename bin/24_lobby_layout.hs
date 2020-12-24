import AdventOfCode (readInputFile)

import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')

data Dir = E | W | NE | NW | SE | SW deriving Show
type Pos2 = (Int, Int)

steps :: Int
steps = 100

life :: Int -> [Int] -> [Int]
life width actives = IntMap.keys (IntMap.filter (\n -> 3 <= n && n <= 5) neighAndSelf)
  where neighAndSelf = IntMap.fromListWith (+) (concatMap oneAndTwos actives)
        oneAndTwos pos = (pos, 1 :: Int) : [(pos + dpos, 2) | dpos <- [-1, 1, -width, width, -width - 1, width + 1]]

coord :: [Dir] -> Pos2
coord = foldl' move (0, 0)
  where move (y, x) E = (y + 1, x + 1)
        move (y, x) W = (y - 1, x - 1)
        move (y, x) NE = (y + 1, x)
        move (y, x) NW = (y, x - 1)
        move (y, x) SE = (y, x + 1)
        move (y, x) SW = (y - 1, x)

parse :: String -> [Dir]
parse "" = []
parse ('e':xs) = E : parse xs
parse ('w':xs) = W : parse xs
parse ('n':'e':xs) = NE : parse xs
parse ('n':'w':xs) = NW : parse xs
parse ('s':'e':xs) = SE : parse xs
parse ('s':'w':xs) = SW : parse xs
parse s = error ("bad " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let coords = map (coord . parse) (lines s)
      xmin = minimum (map snd coords)
      xmax = maximum (map snd coords)
      ymin = minimum (map fst coords)
      width = xmax - xmin + 1 + steps * 2
      compress (y, x) = (y - ymin + steps) * width + x - xmin + steps
      posFlips = map (\pos -> (compress pos, True)) coords
      flips = IntMap.fromListWith (/=) posFlips
      black0 = IntMap.keys (IntMap.filter id flips)
  print (length black0)

  let black100 = iterate (life width) black0 !! steps
  print (length black100)

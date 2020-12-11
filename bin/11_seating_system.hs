{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import qualified Data.Array as Arr
import Data.Array (Array, bounds, listArray)
import qualified Data.Array.Unboxed as UArr
import Data.Array.Unboxed (UArray, accumArray)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (elemIndices, findIndices)
import Data.Maybe (catMaybes)

-- Unlike some other Games of Life that have appeared over the years,
-- in this one, 0 neighbours will cause an empty seat to become occupied.
-- So, it's not sufficient to only check neighbours of occupied seats.
-- All non-floor seats should be checked.
-- (There is an optimisation based on seats that will always remain occupied or empty,
-- but I am not excited about implementing the necessary bookkeeping in Haskell)
stepSeats :: [Int] -> Int -> Array Int [Int] -> [Int] -> [Int]
stepSeats nonFloors thresh neighs occupied = filter alive nonFloors
  where neighAndSelf = neighsArr neighs occupied
        alive pos = ns == 0 || odd ns && ns < thresh * 2
          where ns = neighAndSelf UArr.! pos

neighsArr :: Array Int [Int] -> [Int] -> UArray Int Int
neighsArr neighs = accumArray (+) 0 (bounds neighs) . concatMap oneAndTwos
  where oneAndTwos pos = (pos, 1) : map (, 2) (neighs Arr.! pos)

-- much slower, uncomment the relevant stuff to see just how much.
-- strict map helps a little, but not as much as array.
--  where neighAndSelf = neighsMap neighs occupied
--          where ns = IntMap.findWithDefault 0 pos neighAndSelf
neighsMap, _nm :: Array Int [Int] -> [Int] -> IntMap Int
neighsMap neighs = IntMap.fromListWith (+) . concatMap oneAndTwos
  where oneAndTwos pos = (pos, 1) : map (, 2) (neighs Arr.! pos)
_nm = neighsMap

adjacentNonFloors :: IntSet -> (Int, Int) -> Int -> Int -> [(Int, Int)]
adjacentNonFloors floors (y, x) height width =
  [(ny, nx) | ny <- yrange, nx <- xrange, (ny, nx) /= (y, x), (ny * width + nx) `IntSet.notMember` floors]
  where (yrange, xrange) = ranges (height - 1) (width - 1) y x

visibleNonFloors :: IntSet -> (Int, Int) -> Int -> Int -> [(Int, Int)]
visibleNonFloors floors (y, x) height width =
  catMaybes [look (y + dy, x + dx) (dy, dx) | dy <- [-1 .. 1], dx <- [-1 .. 1], (dy, dx) /= (0, 0)]
  where look :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
        look (ny, _) _ | ny < 0 = Nothing
        look (ny, _) _ | ny >= height = Nothing
        look (_, nx) _ | nx < 0 = Nothing
        look (_, nx) _ | nx >= width = Nothing
        look (ny, nx) (dy, dx) | (ny * width + nx) `IntSet.member` floors = look (ny + dy, nx + dx) (dy, dx)
        look pos _ = Just pos

ranges :: Int -> Int -> Int -> Int -> ([Int], [Int])
ranges maxY maxX y x = ([yMin..yMax], [xMin..xMax])
  where yMin = max (y - 1) 0
        yMax = min (y + 1) maxY
        xMin = max (x - 1) 0
        xMax = min (x + 1) maxX

untilEq :: Eq a => (a -> a) -> a -> a
untilEq f = fst . until2 (==) f

until2 :: (a -> a -> Bool) -> (a -> a) -> a -> (a, a)
until2 p f x = let x' = f x in if p x x' then (x, x') else until2 p f x'

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "uniform of empty"
uniform f (x:xs) = let y = f x in
  if all ((== y) . f) xs then y else error "non-uniform"

main :: IO ()
main = do
  s <- readInputFile
  let rows = lines s
      seats = concat rows
      width = uniform length rows
      height = length rows
      size = width * height
      maxY = height - 1
      maxX = width - 1
      pos (y, x) = y * width + x
      floors = IntSet.fromDistinctAscList (elemIndices '.' seats)
      nonFloors = findIndices (/= '.') seats
      occupied = elemIndices '#' seats
      neighBy f = listArray (0, size - 1) [map pos (f floors (y, x) height width) | y <- [0 .. maxY], x <- [0 .. maxX]]
  let eq1 = untilEq (stepSeats nonFloors 4 (neighBy adjacentNonFloors)) occupied
  print (length eq1)

  let eq2 = untilEq (stepSeats nonFloors 5 (neighBy visibleNonFloors)) occupied
  print (length eq2)

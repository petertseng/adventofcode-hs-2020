{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Arrow ((***), second)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find, foldl', transpose, unfoldr)
import qualified Data.Set as Set

-- part 1

type Tile = (Int, [String])

cornersAndEdges :: [Tile] -> ([Tile], [Tile], IntMap [Int])
cornersAndEdges tiles = (corners, edges, neighFromBorders borderPatterns)
  where tilesAndNorms = map (\t@(_, i) -> (t, normBorders i)) tiles
        normsAndIds = concatMap (\((tid, _), n) -> map (, [tid]) n) tilesAndNorms
        borderPatterns = IntMap.fromListWith (++) normsAndIds
        tileUniqueCounts = map (second (count uniqueBorder)) tilesAndNorms
        uniqueBorder pat = length (borderPatterns IntMap.! pat) == 1
        tilesWithUnique n = map fst (filter ((== n) . snd) tileUniqueCounts)
        corners = tilesWithUnique 2
        edges = tilesWithUnique 1

-- given a map of borders -> [tile IDs that have that border],
-- give a map of tile ID -> [tile IDs of its neighbours]
neighFromBorders :: IntMap [Int] -> IntMap [Int]
neighFromBorders m = IntMap.fromListWith (++) (concatMap pairIfTwo (IntMap.elems m))
  where pairIfTwo [_] = []
        pairIfTwo [x, y] = [(x, [y]), (y, [x])]
        pairIfTwo _ = error "more than two"

transforms :: [[a]] -> [[[a]]]
transforms s = [
    s
  , reverse s
  , flipv s
  , flipv (reverse s)
  , transpose s
  , transpose (reverse s)
  , transpose (flipv s)
  , transpose (flipv (reverse s))
  ]
  where flipv = map reverse

topBorder, bottomBorder, leftBorder, rightBorder :: [String] -> String
topBorder = head
bottomBorder = last
leftBorder = map head
rightBorder = map last

normBorders :: [String] -> [Int]
normBorders b = map norm1 [topBorder, bottomBorder, leftBorder, rightBorder]
  where norm1 f = let s = f b in (min `on` bits) s (reverse s)

bits :: String -> Int
bits = foldl' (\a c -> a * 2 + (if c == '#' then 1 else 0)) 0

-- part 2 steps:
-- build grid (identify which tiles go where, but not how they should be oriented)
-- orient grid (rotate + flip the tiles so they line up)
-- remove borders
-- count monsters

-- part 2 step: build grid

-- build the entire outer edge, then build row-by-row
-- why did I build the entire outer edge first? It was convenient
buildGrid :: IntMap [Int] -> [Int] -> [Int] -> Int -> [[Int]]
buildGrid neigh corners edges topLeft = zipWith (\a b -> a ++ [b]) gridMinusBottomAndRight (topRight : rightEdge) ++ [bottomRow]
  where gridMinusBottomAndRight = buildGridFromEdges neigh topRow leftEdge
        -- this contains the entire border minus the top left corner
        border = buildBorder neigh (`IntSet.member` edgesOrCorners) topLeft
        edgesOrCorners = IntSet.fromList (corners ++ edges)
        -- corners only has 4 elements so I think `elem` should be fine here?
        (topEdge, topRight, rightBottomLeft) = case break (`elem` corners) border of
          (te, tr:rbl) -> (te, tr, rbl)
          (_, []) -> error "no corner"
        topRow = topLeft : topEdge ++ [topRight]
        width = length topRow
        (rightEdge, bottomAndLeft) = break (`elem` corners) rightBottomLeft
        (bottomRow, leftEdge) = (reverse *** reverse) (splitAt width bottomAndLeft)

buildBorder :: IntMap [Int] -> (Int -> Bool) -> Int -> [Int]
buildBorder neigh accept t = unfoldr nextTile (0, t)
  -- I'd write and use an atMostOne here, but the top left corner has two.
  where nextTile (prev, curr) = fmap (\t' -> (t', (curr, t'))) (find acceptUnused (neigh IntMap.! curr))
          where acceptUnused t' = accept t' && t' /= prev && t' /= t

buildGridFromEdges :: IntMap [Int] -> [Int] -> [Int] -> [[Int]]
buildGridFromEdges neigh topRow = scanl (buildRowBelow neigh) (init topRow)

buildRowBelow :: IntMap [Int] -> [Int] -> Int -> [Int]
buildRowBelow neigh above left' = scanl (rightAndBelow neigh) left' (zip above (tail above))

rightAndBelow :: IntMap [Int] -> Int -> (Int, Int) -> Int
rightAndBelow neigh lft (aboveLeft, above) = exactlyOne (filter otherCorner (neigh IntMap.! above))
  where leftNeigh = neigh IntMap.! lft
        otherCorner t = t `elem` leftNeigh && t /= aboveLeft

-- part 2 step: orient grid

-- find an orientation for the top-left that matches the tiles below and to the right,
-- orient the top row by considering the tile to its left
-- orient all rows below by considering the tile above
orientGrid :: IntMap [String] -> [String] -> [[Int]] -> [[[String]]]
orientGrid tiles topLeft grid = scanl orientRowBelow topRowOriented belowRows
  where topRowOriented = scanl orientRight topLeftOrientation topRow
        topRow :: [[String]]
        topRow = map (tiles IntMap.!) (tail (head grid))
        tile01 = tiles IntMap.! (head grid !! 1)
        tile10 = tiles IntMap.! head (grid !! 1)
        -- Candidates, considering just tile to its right (should be two of these)
        topLeftCandidates = [tlo | tlo <- transforms topLeft, or01 <- transforms tile01, rightBorder tlo == leftBorder or01]
        -- Will either need to flip vertically or not to match tile below.
        -- I did these two sequentially so as to not waste time checking some useless combinations,
        -- but I wonder if the compiler would be smart enough to not check them anyway?
        topLeftOrientation = exactlyOne [tlo | tlo <- topLeftCandidates, or10 <- transforms tile10, bottomBorder tlo == topBorder or10]
        belowRows :: [[[String]]]
        belowRows = map (map (tiles IntMap.!)) (tail grid)

orientRowBelow :: [[String]] -> [[String]] -> [[String]]
orientRowBelow = zipWith orientBelow

orientPair :: ([String] -> String) -> ([String] -> String) -> [String] -> [String] -> [String]
orientPair fa fb a b = exactlyOne (filter match (transforms b))
  where match b' = fb b' == a'
        a' = fa a

orientBelow, orientRight :: [String] -> [String] -> [String]
orientBelow = orientPair bottomBorder topBorder
orientRight = orientPair rightBorder leftBorder

-- part 2 step: remove borders

rmBorders :: [[[String]]] -> [String]
rmBorders = concatMap rmBordersRow

rmBordersRow :: [[String]] -> [String]
rmBordersRow tiles = map concat (transpose tileRows)
  where tileRows = map rmBordersTile tiles

rmBordersTile :: [String] -> [String]
rmBordersTile = middle . map middle
  where middle = tail . init

-- part 2 step: count monsters

type Pos2 = (Int, Int)

tilesPerMonster :: Int
tilesPerMonster = 15

monsters :: [Pos2] -> Int
monsters pounds = sum (map monster pounds)
 where monster = count (all (`Set.member` poundsSet)) . transformedMonsters
       transformedMonsters pos = [transformedMonsterPoints pos dy dx f | dy <- [-1, 1], dx <- [-1, 1], f <- [swap, id]]
       poundsSet = Set.fromList pounds
       swap (x, y) = (y, x)

transformedMonsterPoints :: Pos2 -> Int -> Int -> (Pos2 -> Pos2) -> [Pos2]
transformedMonsterPoints (y, x) dy dx f = [(y + y', x + x') | (y', x') <- map (f . ((* dy) *** (* dx))) monsterPoints]

monsterPoints :: [Pos2]
monsterPoints = [
    (-1, 18)
  -- , (0, 0)
  , (0, 5)
  , (0, 6)
  , (0, 11)
  , (0, 12)
  , (0, 17)
  , (0, 18)
  , (0, 19)
  , (1, 1)
  , (1, 4)
  , (1, 7)
  , (1, 10)
  , (1, 13)
  , (1, 16)
  ]

-- general

enumGrid :: [[a]] -> [(Pos2, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

exactlyOne :: [a] -> a
exactlyOne [x] = x
exactlyOne [] = error "none for exactlyOne"
exactlyOne (_:_) = error "too many for exactlyOne"

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

-- parse

parseTile :: [String] -> Tile
parseTile (x:xs) = (parseTileId x, xs)
parseTile _ = error "empty tile"

parseTileId :: String -> Int
parseTileId ('T':'i':'l':'e':' ':a:b:c:d:":") = read [a, b, c, d]
parseTileId s = error ("bad tile id " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let tiles = map parseTile (splitOn "" (lines s))
      (corners, edges, neigh) = cornersAndEdges tiles
  print (product (map fst corners))

  let (topLeftTid, topLeftImg) = head corners
      grid = buildGrid neigh (map fst corners) (map fst edges) topLeftTid
  --print grid

  let oriented = orientGrid (IntMap.fromList tiles) topLeftImg grid
      img = rmBorders oriented
      pounds = map fst (filter ((== '#') . snd) (enumGrid img))
  print (length pounds - tilesPerMonster * monsters pounds)

{-# LANGUAGE NumericUnderscores #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Monad (foldM, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.List (elemIndices)
import Data.Maybe (listToMaybe)

-- Huh, using Word32 makes this slower... wonder why.
game :: [Int] -> Int -> Int
game start rounds = runST $ do
  lastSpokenAt <- newArray (0, rounds) (-1) :: ST s (STUArray s Int Int)

  zipWithM_ (writeArray lastSpokenAt) start [1..]

  foldM (\spokenNow t -> do
      tprev <- readArray lastSpokenAt spokenNow
      writeArray lastSpokenAt spokenNow t
      return (if tprev == -1 then 0 else t - tprev)
    ) (spokenAfterStart start) [(length start + 1) .. (rounds - 1)]

spokenAfterStart :: [Int] -> Int
spokenAfterStart start = case lastIndex (last start) (init start) of
  Nothing -> 0
  Just i -> length start - 1 - i

lastIndex :: Eq a => a -> [a] -> Maybe Int
lastIndex x = listToMaybe . reverse . elemIndices x

main :: IO ()
main = do
  s <- readInputFile
  let start = map read (splitOn ',' s)
  {- HLINT ignore main "Use underscore" -}
  print (game start 2020)
  print (game start 30_000_000)

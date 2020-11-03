{-# LANGUAGE TupleSections #-}

module AdventOfCode (
  readInputFile
, readInputFileAndFlags
) where

import Data.Char (isAlphaNum)
import Data.List (partition)
import System.Environment (getArgs)

readInputFile :: IO String
readInputFile = do
  args <- getArgs
  let f = case args of
        [] -> "/dev/stdin"
        a:_ -> a
  readFile f

isFlag :: String -> Bool
isFlag ('-':x:_) = isAlphaNum x
isFlag _ = False

readInputFileAndFlags :: IO (String, [(Char, String)])
readInputFileAndFlags = do
  args <- getArgs
  let (flags, notFlags) = partition isFlag args
  let f = case notFlags of
        [] -> "/dev/stdin"
        a:_ -> a
  fmap (, map (\v -> (v !! 1, drop 2 v)) flags) (readFile f)

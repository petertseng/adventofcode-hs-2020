{-# LANGUAGE NumericUnderscores #-}

import AdventOfCode (readInputFile)

import Control.Monad (foldM_, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Char (digitToInt, isDigit)

game :: [Int] -> Int -> Int -> Int -> [Int]
game cups ncups nrounds nRightOf1 = tail $ runST $ do
  right <- newListArray (1, ncups) [2 .. ncups + 1] :: ST s (STUArray s Int Int)
  zipWithM_ (writeArray right) cups (tail cups)
  if ncups > length cups
    then do
      writeArray right (last cups) (length cups + 1)
      writeArray right ncups (head cups)
    else writeArray right (last cups) (head cups)

  foldM_ (\current _ -> do
      pickup1 <- readArray right current
      pickup2 <- readArray right pickup1
      pickup3 <- readArray right pickup2
      afterPickup <- readArray right pickup3

      let dest = until1 (\d -> d /= pickup1 && d /= pickup2 && d /= pickup3) (\d -> if d == 1 then ncups else d - 1) current
      rightOfDest <- readArray right dest

      writeArray right current afterPickup
      writeArray right dest pickup1
      writeArray right pickup3 rightOfDest

      return afterPickup
    ) (head cups) [1 .. nrounds]

  scanM (\current _ -> readArray right current) 1 [1 .. nRightOf1]

scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM _ q [] = return [q]
scanM f q (x:xs) =
   do q2 <- f q x
      qs <- scanM f q2 xs
      return (q:qs)

until1 :: (a -> Bool) -> (a -> a) -> a -> a
until1 p f a = until p f (f a)

main :: IO ()
main = do
  s <- readInputFile
  let cups = map digitToInt (filter isDigit s)
  putStrLn (concatMap show (game cups 9 100 8))

  let xs = game cups 1_000_000 10_000_000 2
  print (product xs)

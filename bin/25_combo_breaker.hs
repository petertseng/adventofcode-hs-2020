import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Prelude hiding (exp)

modulus :: Int
modulus = 20201227

discreteLog :: Int -> Int -> Int -> Int
discreteLog g h modu = find h 0
  where find _ i | i >= m = error "no discrete log"
        find gamma i = case IntMap.lookup gamma jOfAj of
          Just j  -> i * m + j
          Nothing -> find ((gamma * am) `mod` modu) (i + 1)
        m = ceiling (sqrt (fromIntegral' modu))
        jOfAj = IntMap.fromList (zip (iterate ((`mod` modu) . (* g)) 1) [0 .. m - 1])
        am = modPow g (modu - m - 1) modu
        fromIntegral' :: Int -> Double
        fromIntegral' = fromIntegral

modPow :: Int -> Int -> Int -> Int
modPow _ 0 _ = 1
modPow b exp m = modPow' b 1 exp m

modPow' :: Int -> Int -> Int -> Int -> Int
modPow' evens odds exp m | exp < 2 = evens * odds `mod` m
modPow' evens odds exp m = modPow' evens' odds' exp' m
  where evens' = evens *~ evens
        odds'  = if odd exp then odds *~ evens else odds
        exp'   = exp `div` 2
        a *~ b = a * b `mod` m

findNumbers :: String -> [Int]
findNumbers = mapMaybe readMaybe . words

main :: IO ()
main = do
  args <- getArgs
  let keys s = let [a, b] = findNumbers s in return (a, b)
  (pub1, pub2) <- case args of
    a:b:_ -> return (read a, read b)
    f:_ -> readFile f >>= keys
    _ -> readFile "/dev/stdin" >>= keys

  let priv1 = discreteLog 7 pub1 modulus
  print (modPow pub2 priv1 modulus)

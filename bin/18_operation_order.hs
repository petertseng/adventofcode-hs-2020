import AdventOfCode (readInputFile)

import Data.Char (digitToInt, isDigit)
import Data.Either (isLeft)
import Data.List (foldl')

parse :: (Char -> Int) -> (Char -> Either () ()) -> String -> Int
parse prec assoc = oneVal . popAll . foldl' parse' ([], [])
  where parse' :: ([Int], [Char]) -> Char -> ([Int], [Char])
        parse' (nums, ops) d | isDigit d = (digitToInt d : nums, ops)
        parse' (nums, ops) op | op `elem` "+*" =
          let keepPop (_, '(':_) = False
              keepPop (_, op':_) | prec op' > prec op = True
              keepPop (_, op':_) | prec op' == prec op && isLeft (assoc op) = True
              keepPop _ = False
              (nums', ops') = until (not . keepPop) popOp (nums, ops)
          in (nums', op:ops')
        parse' (nums, ops) '(' = (nums, '(':ops)
        parse' no ')' = case until lParen popOp no of
          (nums', '(':ops') -> (nums', ops')
          _ -> error "bad rparen"
          where lParen (_, '(':_) = True
                lParen (_, _:_) = False
                lParen (_, []) = error "bad lParen"
        parse' no ' ' = no
        parse' _ c = error ("bad char " ++ [c])
        popOp (a:b:nums, '+':ops) = (a+b:nums, ops)
        popOp (a:b:nums, '*':ops) = (a*b:nums, ops)
        popOp _ = error "bad pop"
        popAll = until (null . snd) popOp
        oneVal ([num], []) = num
        oneVal _ = error "bad oneVal"

-- old hand-crafted way
ltor, _ltor :: String -> Int
ltor s = case foldl' ltor' [(Nothing, Nothing)] s of
  [(Just v, _)] -> v
  _ -> undefined
_ltor = ltor

ltor' :: [(Maybe Int, Maybe Char)] -> Char -> [(Maybe Int, Maybe Char)]
ltor' stk c = case (stk, c) of
  (_, '(') -> (Nothing, Nothing):stk
  ((Just v, Just op):xs, d) | isDigit d -> (Just (ap op v (digitToInt d)), Nothing) : xs
  (_:xs, d) | isDigit d -> (Just (digitToInt d), Nothing) : xs
  ((v, _):xs, op) | op `elem` "+*" -> (v, Just op) : xs
  ((Just v1, _):(Just v2, Just op):xs, ')') -> (Just (ap op v2 v1), Nothing) : xs
  ((Just v1, _):_:xs, ')') -> (Just v1, Nothing) : xs
  (xs, ' ') -> xs
  (_, c') -> error ("bad char " ++ [c'])

ap :: Char -> Int -> Int -> Int
ap '+' x y = x + y
ap '*' x y = x * y
ap op _ _ = error ("bad op " ++ [op])

main :: IO ()
main = do
  s <- readInputFile
  let equations = lines s
      parse1 = parse (const 0) (const (Left ()))
  print (sum (map parse1 equations))
  let prec op = case op of
        '+' -> 1
        '*' -> 0
        _ -> error ("bad op " ++ [op])
      parse2 = parse prec (const (Left ()))
  print (sum (map parse2 equations))

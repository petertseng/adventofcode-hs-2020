import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Data.Array ((!), (//), Array, array)

data Rule = A | B | Ref Int | Seq Rule Rule | Alt Rule Rule

fullMatchRule :: Array Int Rule -> Int -> String -> Bool
fullMatchRule rules n s = "" `elem` matchRule rules (rules ! n) s

matchRule :: Array Int Rule -> Rule -> String -> [String]
matchRule rules rule s = case (rule, s) of
  (A, 'a':x) -> [x]
  (A, _) -> []
  (B, 'b':x) -> [x]
  (B, _) -> []
  (Ref n', _) -> matchRule rules (rules ! n') s
  (Seq r1 r2, _) -> matchRule rules r1 s >>= matchRule rules r2
  (Alt r1 r2, _) -> matchRule rules r1 s ++ matchRule rules r2 s

parseRule :: String -> (Int, Rule)
parseRule s = let (l, r) = splitOnOne ':' s in (read l, parseRhs (tail r))

parseRhs :: String -> Rule
parseRhs "\"a\"" = A
parseRhs "\"b\"" = B
parseRhs s | '|' `elem` s = case words s of
  [a, b, "|", c, d] -> Alt (seqOfRefs a b) (seqOfRefs c d)
  [a, "|", b] -> Alt (Ref (read a)) (Ref (read b))
  _ -> error ("bad rule " ++ s)
parseRhs s = case words s of
  [x] -> Ref (read x)
  [x, y] -> seqOfRefs x y
  [x, y, z] -> Seq (Ref (read x)) (seqOfRefs y z)
  _ -> error ("bad rule " ++ s)

seqOfRefs :: String -> String -> Rule
seqOfRefs a b = Seq (Ref (read a)) (Ref (read b))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let (rulesStr, messages) = splitOnOne "" (lines s)
      rules = map parseRule rulesStr
      ruleNums = map fst rules
      rulesArr = array (0, maximum ruleNums) rules
  print (count (fullMatchRule rulesArr 0) messages)

  let canPart2 = 42 `elem` ruleNums && 31 `elem` ruleNums
      rule8 = Alt (Ref 42) (Seq (Ref 42) (Ref 8))
      rule11 = Seq (Ref 42) (Alt (Ref 31) (Seq (Ref 11) (Ref 31)))
      rulesArr' = rulesArr // [(8, rule8), (11, rule11)]
  if canPart2
    then print (count (fullMatchRule rulesArr' 0) messages)
    else putStrLn "no part 2"

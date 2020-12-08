import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

import Data.Array ((!), (//), Array, bounds, listArray)
import Data.Either (rights)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (mapMaybe)

data Inst = Acc Int | Jmp Int | Nop Int
type Insts = Array Int Inst
data Status = Loop | Term | BadPC deriving (Eq, Show)

-- Status, PC, Acc
run :: Insts -> IntSet -> (Status, Int, Int)
run insts canMutateTo = run' IntSet.empty 0 0 True
  where len = snd (bounds insts) + 1
        run' :: IntSet -> Int -> Int -> Bool -> (Status, Int, Int)
        run' _ acc pc _ | pc == len = (Term, pc, acc)
        run' _ acc pc _ | pc < 0 || pc > len = (BadPC, pc, acc)
        run' seen acc pc _ | pc `IntSet.member` seen = (Loop, pc, acc)
        run' seen acc pc mut = run' seen' acc' pc' mut'
          where seen' = IntSet.insert pc seen
                (acc', pcNoMut, pcMut) = case insts ! pc of
                  Acc v -> (acc + v, pc + 1, Nothing)
                  Jmp v -> (acc, pc + v, Just (pc + 1))
                  Nop v -> (acc, pc + 1, Just (pc + v))
                (mut', pc') = case (mut, pcMut) of
                  (True, Just pm) | pm `IntSet.member` canMutateTo -> (False, pm)
                  (m, _) -> (m, pcNoMut)

cfg :: [Inst] -> IntMap [Int]
cfg = IntMap.fromListWith (++) . zipWith comeFrom [0..]
  where comeFrom pc inst = (step inst pc, [pc])
        step (Acc _) = succ
        step (Jmp v) = (+) v
        step (Nop _) = succ

terminatingPcs :: [Inst] -> [Int]
terminatingPcs insts = map snd gensAndGoals
  where gensAndGoals = rights (bfs (flip (IntMap.findWithDefault []) comeFrom) (const True) (length insts))
        comeFrom = cfg insts

terminatingAccs, _tas :: Insts -> [Int]
terminatingAccs insts = mapMaybe accIfTerm runs
  where swappeds = mapMaybe (swap insts) [0 .. (len - 1)]
        (_, len) = bounds insts
        runs = map (`run` IntSet.empty) swappeds
_tas = terminatingAccs

accIfTerm :: (Status, a, b) -> Maybe b
accIfTerm (Term, _, acc) = Just acc
accIfTerm _ = Nothing

swap :: Insts -> Int -> Maybe Insts
swap insts i = fmap (insertAt i) (swap' (insts ! i))
  where insertAt j inst = insts // [(j, inst)]
        swap' (Acc _) = Nothing
        swap' (Jmp v) = Just (Nop v)
        swap' (Nop v) = Just (Jmp v)

parseInst :: String -> Inst
parseInst s = op (read' v) where
  (op, v) = case words s of
    ["acc", v'] -> (Acc, v')
    ["jmp", v'] -> (Jmp, v')
    ["nop", v'] -> (Nop, v')
    _ -> error ("bad inst " ++ s)
  read' ('+':v') = read v'
  read' v' = read v'

main :: IO ()
main = do
  s <- readInputFile
  let instsL = map parseInst (lines s)
      insts = listArray (0, length instsL - 1) instsL
      (stat, pc, acc) = run insts IntSet.empty
  putStrLn (if stat == Loop then show acc else show (stat, pc))

  let termPcs = IntSet.fromList (terminatingPcs instsL)
      (stat', pc', acc') = run insts termPcs
  putStrLn (if stat' == Term then show acc' else show (stat', pc'))
  --mapM_ print (terminatingAccs insts) -- mapM_ is in Prelude and traverse_ isn't, so using mapM means I don't have to uncomment an import

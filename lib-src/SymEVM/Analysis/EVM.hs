module SymEVM.Analysis.EVM where

import Data.Array
import Data.Either

import SymEVM.Data

import qualified SymEVM.Data.Util.Set as S

-- | Classifies states as either buggy (`Err`) or valid. If a state is valid, simply injects input state into `Right`.
--   This is where all types of bug detection should live. For example, to check for divide by zero errors, add a case
--   for divide by zero.
existsBug :: State -> Either Err State
existsBug st = Right st

incrPC :: State -> State
incrPC st = 
  let machine' = machine st  in
  let pc'      = pc machine' in
  st { machine = machine' { pc = pc' + 1 } }

-- | Produces the set of all next possible states. For concrete states, result will always be a set of size 1 which contains
--   the next state. For symbolic states, there could be many possible next states (e.g. multiple jump destinations).
step :: State -> S.Set State
step st =
  case instr of
    0x00 ->
      let st' = incrPC st in
      S.singleton st'
  where
    instr = (code . env $ st) ! (pc . machine $ st)

stepBug :: State -> S.Set (Either Err State)
stepBug st = S.map existsBug (step st)

stepErr :: State -> (S.Set Err, S.Set State)
stepErr st = partitionEithers (stepBug st)
  where
    partitionEithers eithers = S.foldl partitionEither (S.empty, S.empty) eithers -- TODO: Put in Data.Either extension module
    partitionEither acc curr = case curr of
                                 Left  val -> (S.insert val (fst acc), snd acc)
                                 Right val -> (fst acc, S.insert val (snd acc))

eval' :: (S.Set Err, S.Set State) -> (S.Set Err, S.Set State)
eval' curr =
  let errs  = fst curr                         in
  let sts   = snd curr                         in
  let tmp   = flattenPairs (S.map stepErr sts) in
  let errs' = fst tmp                          in
  let sts'  = snd tmp                          in
  (S.union errs errs', sts')
  where
    flattenPairs pairs   = S.foldl flattenPair (S.empty, S.empty) pairs -- TODO: Put in utility module
    flattenPair acc curr = (S.union (fst acc) (fst curr), S.union (snd acc) (snd curr))

iter :: (S.Set Err, S.Set State) -> (S.Set Err, S.Set State)
iter curr =
  let sts = snd curr in
  if S.null sts then
    curr
  else
    iter (eval' curr)

eval :: State -> S.Set Err
eval st = fst (iter (S.empty, S.singleton st))

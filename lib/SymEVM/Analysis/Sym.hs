module SymEVM.Analysis.Sym where

import Data.Array
import Data.Either

import SymEVM.Data.Sym.State

import SymEVM.Data.Sym.Machine
import SymEVM.Data.Common.ProgCnt

import SymEVM.Data.Sym.Env
import SymEVM.Data.Common.Code

import SymEVM.Data.Sym.Err

-- Takes a (possibly invalid) symbolic state and determines if its good or bad.
--
-- NOTE: This is where error detection goes (e.g. div by zero)
existsBug :: State -> Either Err State
existsBug st = Right st

incrPC :: State -> State
incrPC st = 
  let machine' = machine st  in
  let pc'      = pc machine' in
  st { machine = machine' { pc = pc' + 1 } }

-- Takes a state and performs a single step of symbolic execution
-- NOTE: This function assumes input state is well-formed (all possible steps are valid)
step :: State -> [State]
step st =
  case instr of
    0x00 ->
      let st' = incrPC st in
      return st'
  where
    instr = (code . env $ st) ! (pc . machine $ st)

stepBug :: State -> [Either Err State]
stepBug st = map existsBug (step st)

stepErr :: State -> ([Err], [State])
stepErr st = partitionEithers (stepBug st)

eval' :: ([Err], [State]) -> ([Err], [State])
eval' curr =
  let errs = fst curr in
  let sts  = snd curr in
  let tmp  = collect (map stepErr sts) in
  let errs' = fst tmp in
  let sts'  = snd tmp in
  (errs ++ errs', sts')
  where
    collect xs = foldl (\acc curr -> ((fst acc) ++ (fst curr), (snd acc) ++ (snd curr))) ([], []) xs

fix :: Eq a => (a -> a) -> a -> a
fix f x =
  let x' = f x in
  if x == x' then
    x
  else
    fix f (f x)

eval :: State -> [Err]
eval st = fst (fix eval' ([], [st]))




-- import Data.Array
-- 
-- import SymEVM.Data.StackFrame
-- import SymEVM.Data.Symbol
-- import SymEVM.Data.State
-- 
-- import Control.Monad.State(StateT)
-- 
-- 
-- 
-- updatePC :: State -> Integer -> State
-- updatePC st = 
--   let machine' = machine st in
--   let pc' = pc machine' in
--   st { machine = machine' { pc = pc' } }
-- 
-- pop :: State -> (StackFrame, State)
-- pop st =
--   let machine' = machine st in
--   let stack'   = stack machine' in
--   let (head : tail) = stack' in
--   (head, st { machine = machine' { stack = tail } })
-- 
-- step :: State -> StateT Integer [] State
-- step st =
--   case instr of
--     0x00 ->
--       let st' = updatePC st in
--       return st
--     0x01 ->
--       let (s0, st')  = pop st  in
--       let (s1, st'') = pop st' in
--       return st''
--   where
--     instr = (code . env $ st) ! (pc . machine $ st)

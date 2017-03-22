module SymEVM.Analysis.Sym where

import Data.Array

import SymEVM.Data.StackFrame
import SymEVM.Data.Symbol
import SymEVM.Data.State

import Control.Monad.State(StateT)

updatePC :: State -> State
updatePC st = 
  let machine' = machine st in
  let pc' = pc machine' in
  st { machine = machine' { pc = pc' } }

pop :: State -> (StackFrame, State)
pop st =
  let machine' = machine st in
  let stack'   = stack machine' in
  let (head : tail) = stack' in
  (head, st { machine = machine' { stack = tail } })

step :: State -> StateT Integer [] State
step st =
  case instr of
    0x00 ->
      let st' = updatePC st in
      return st
    0x01 ->
      let (s0, st')  = pop st  in
      let (s1, st'') = pop st' in
      return st''
  where
    instr = (code . env $ st) ! (pc . machine $ st)

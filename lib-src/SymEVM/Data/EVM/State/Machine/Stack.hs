module SymEVM.Data.EVM.State.Machine.Stack 
  ( module SymEVM.Data.Util.Symbol
  , StackFrame
  , Stack
  ) where

import SymEVM.Data.Util.Symbol

type StackFrame = Symbol

type Stack = [StackFrame]

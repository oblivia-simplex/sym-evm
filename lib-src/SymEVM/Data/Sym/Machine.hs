module SymEVM.Data.Sym.Machine where

import SymEVM.Data.Common.ProgCnt
import SymEVM.Data.Sym.Stack
import SymEVM.Data.Sym.StackFrame

data Machine = Machine
  { pc    :: ProgCnt
  , stack :: Stack StackFrame
  } deriving ( Eq, Ord )

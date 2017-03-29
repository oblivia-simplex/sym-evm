module SymEVM.Data.Sym.Machine where

import SymEVM.Data.Common.ProgCnt
import SymEVM.Data.Sym.Stack

data Machine = Machine
  { pc    :: ProgCnt
  , stack :: Stack
  } deriving ( Eq )

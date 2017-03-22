module SymEVM.Data.Machine where

import SymEVM.Data.ProgramCounter
import SymEVM.Data.Stack

data Machine = Machine
  { pc    :: ProgramCounter
  , stack :: Stack
  }

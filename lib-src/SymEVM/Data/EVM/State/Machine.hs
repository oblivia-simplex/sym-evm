module SymEVM.Data.EVM.State.Machine 
  ( module SymEVM.Data.EVM.State.Machine.ProgCnt
  , module SymEVM.Data.EVM.State.Machine.Stack
  , Machine(..)
  ) where

import SymEVM.Data.EVM.State.Machine.ProgCnt
import SymEVM.Data.EVM.State.Machine.Stack

data Machine = Machine
  { pc    :: ProgCnt
  , stack :: Stack
  } deriving ( Eq, Ord )

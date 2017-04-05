module SymEVM.Data.EVM.State.Machine 
  ( module SymEVM.Data.EVM.State.Machine.ProgCnt
  , module SymEVM.Data.EVM.State.Machine.Stack
  , Machine(..)
  , pc
  , stack
  ) where

import Control.Lens

import SymEVM.Data.EVM.State.Machine.ProgCnt
import SymEVM.Data.EVM.State.Machine.Stack

data Machine = Machine
  { _pc    :: ProgCnt
  , _stack :: Stack
  } deriving ( Eq, Ord )

pc :: Lens' Machine ProgCnt
pc = lens _pc (\machine newPc -> machine { _pc = newPc })

stack :: Lens' Machine Stack
stack = lens _stack (\machine newStack -> machine { _stack = newStack })

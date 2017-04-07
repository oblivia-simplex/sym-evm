module SymEVM.Data.EVM.State.Env.Block where

import SymEVM.Data.Util.Symbol

import Control.Lens

data Block = Block
  { _number :: Symbol
  } deriving ( Show, Eq, Ord )

number :: Lens' Block Symbol
number = lens _number (\block newNumber -> block { _number = newNumber })

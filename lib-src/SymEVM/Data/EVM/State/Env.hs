module SymEVM.Data.EVM.State.Env 
  ( module SymEVM.Data.EVM.State.Env.Code
  , Env(..)
  ) where

import SymEVM.Data.EVM.State.Env.Code

data Env = Env
  { code :: Code
  } deriving ( Eq, Ord )

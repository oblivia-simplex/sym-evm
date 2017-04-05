module SymEVM.Data.EVM.State.Env 
  ( module SymEVM.Data.EVM.State.Env.Code
  , Env(..)
  , code
  ) where

import Control.Lens

import SymEVM.Data.EVM.State.Env.Code

data Env = Env
  { _code :: Code
  } deriving ( Eq, Ord )

code :: Lens' Env Code
code = lens _code (\env newCode -> env { _code = newCode })

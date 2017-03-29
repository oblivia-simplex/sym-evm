module SymEVM.Data.Sym.Env where

import SymEVM.Data.Common.Code

data Env = Env
  { code :: Code
  } deriving ( Eq, Ord )

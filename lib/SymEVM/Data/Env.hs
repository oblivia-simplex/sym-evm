module SymEVM.Data.Env where

import SymEVM.Data.Code

data Env = Env
  { code :: Code
  }

module SymEVM.Data.EVM.State.Env 
  ( module SymEVM.Data.EVM.State.Env.Code
  , module SymEVM.Data.EVM.State.Env.Block
  , Env(..)
  , code
  , block
  ) where

import Control.Lens

import SymEVM.Data.EVM.State.Env.Code
import SymEVM.Data.EVM.State.Env.Block

data Env = Env
  { _block :: Block
  , _code  :: Code
  } deriving ( Show, Eq, Ord )

block :: Lens' Env Block
block = lens _block (\env newBlock -> env { _block = newBlock })

code :: Lens' Env Code
code = lens _code (\env newCode -> env { _code = newCode })

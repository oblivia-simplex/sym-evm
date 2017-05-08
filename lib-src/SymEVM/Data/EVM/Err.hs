module SymEVM.Data.EVM.Err where

import SymEVM.Data.EVM.State

data Err = Err State String deriving ( Eq, Ord, Show )

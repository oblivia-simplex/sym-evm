module SymEVM.Data.EVM.Err where

import SymEVM.Data.EVM.State

newtype Err = Err State deriving ( Eq, Ord, Show )

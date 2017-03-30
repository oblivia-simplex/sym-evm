module SymEVM.Data.EVM.State
  ( module SymEVM.Data.EVM.State.World
  , module SymEVM.Data.EVM.State.Machine
  , module SymEVM.Data.EVM.State.Substate
  , module SymEVM.Data.EVM.State.Env
  , State(..)
  ) where

import SymEVM.Data.EVM.State.World
import SymEVM.Data.EVM.State.Machine
import SymEVM.Data.EVM.State.Substate
import SymEVM.Data.EVM.State.Env

data State = State
  { world    :: World
  , machine  :: Machine
  , substate :: Substate
  , env      :: Env
  } deriving ( Eq, Ord )

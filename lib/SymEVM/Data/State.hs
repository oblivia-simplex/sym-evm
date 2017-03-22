module SymEVM.Data.State 
  ( module SymEVM.Data.World
  , module SymEVM.Data.Machine
  , module SymEVM.Data.Substate
  , module SymEVM.Data.Env
  , State(..) 
  ) where

import SymEVM.Data.World
import SymEVM.Data.Machine
import SymEVM.Data.Substate
import SymEVM.Data.Env

data State = State
  { world    :: World
  , machine  :: Machine
  , substate :: Substate
  , env      :: Env
  }

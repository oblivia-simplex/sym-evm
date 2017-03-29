module SymEVM.Data.Sym.State
  ( State(..) 
  ) where

import SymEVM.Data.Sym.World
import SymEVM.Data.Sym.Machine
import SymEVM.Data.Sym.Substate
import SymEVM.Data.Sym.Env

data State = State
  { world    :: World
  , machine  :: Machine
  , substate :: Substate
  , env      :: Env
  } deriving ( Eq )

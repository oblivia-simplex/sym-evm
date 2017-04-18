module SymEVM.Data.EVM.State
  ( module SymEVM.Data.EVM.State.World
  , module SymEVM.Data.EVM.State.Machine
  , module SymEVM.Data.EVM.State.Substate
  , module SymEVM.Data.EVM.State.Env
  , State(..)
  , machine
  , env
  , cond
  ) where

import Control.Lens

import SymEVM.Data.EVM.State.World
import SymEVM.Data.EVM.State.Machine
import SymEVM.Data.EVM.State.Substate
import SymEVM.Data.EVM.State.Env

import SymEVM.Data.Util.Symbol

data State = State
  { world    :: World
  , _machine :: Machine
  , substate :: Substate
  , _env     :: Env
  , _cond    :: Symbol
  } deriving ( Show, Eq, Ord )

machine :: Lens' State Machine
machine = lens _machine (\state newMachine -> state { _machine = newMachine })

env :: Lens' State Env
env = lens _env (\state newEnv -> state { _env = newEnv })

cond :: Lens' State Symbol
cond = lens _cond (\state newCond -> state { _cond = newCond})

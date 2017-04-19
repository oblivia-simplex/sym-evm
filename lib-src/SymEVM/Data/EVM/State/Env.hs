module SymEVM.Data.EVM.State.Env 
  ( module SymEVM.Data.EVM.State.Env.Code
  , module SymEVM.Data.EVM.State.Env.Block
  , Env(..)
  , sender
  , value
  , code
  , block
  ) where

import Control.Lens

import SymEVM.Data.EVM.State.Env.Code
import SymEVM.Data.EVM.State.Env.Block
import SymEVM.Data.Util.Symbol

data Env = Env
  { _sender :: Symbol
  , _value  :: Symbol
  , _code   :: Code
  , _block  :: Block
  } deriving ( Show, Eq, Ord )

sender :: Lens' Env Symbol
sender = lens _sender (\env newSender -> env { _sender = newSender })

value :: Lens' Env Symbol
value = lens _value (\env newValue -> env { _value = newValue })

code :: Lens' Env Code
code = lens _code (\env newCode -> env { _code = newCode })

block :: Lens' Env Block
block = lens _block (\env newBlock -> env { _block = newBlock })

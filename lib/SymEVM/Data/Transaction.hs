module SymEVM.Data.Transaction (Transaction(..)) where

import SymEVM.Prelude

data Transaction = Transaction
    { nonce :: P256
    , gasPrice :: P256
    , gasLimit :: P256
    , to :: Either B20 B0
    , value :: P256
    , payload :: B
    , ecSign :: P5
    , ecR :: P256
    , ecS :: P256
    } deriving ( Show, Eq, Ord )

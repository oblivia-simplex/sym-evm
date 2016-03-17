module SymEVM.Data.Transaction (Transaction(..)) where

import SymEVM.Prelude

data Transaction = Transaction
    { nonce :: Scalar
    , gasPrice :: Scalar
    , gasLimit :: Scalar
    , to :: Either B20 B0
    , value :: Scalar
    } deriving ( Show, Eq, Ord )

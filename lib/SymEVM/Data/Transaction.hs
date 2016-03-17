module SymEVM.Data.Transaction (Transaction(..)) where

import SymEVM.Prelude

import Data.ByteString

data Transaction = Transaction
    { nonce :: S256
    , gasPrice :: S256
    , gasLimit :: S256
    , to :: Either B20 B0
    , value :: S256
    , payload :: ByteString
    , ecSign :: S5
    , ecR :: S256
    , ecS :: S256
    } deriving ( Show, Eq, Ord )

module SymEVM.Prelude.ByteArray (B20, toB20, fromB20, B0, mkB0) where

import Prelude hiding (length)

import Data.ByteString

newtype B20 = B20 { fromB20 :: ByteString } deriving ( Show, Eq, Ord )

toB20 :: ByteString -> Maybe B20
toB20 bs =
    if length bs == 20 then
        Just $ B20 { fromB20 = bs }
    else
        Nothing

newtype B0 = B0 ByteString deriving ( Show, Eq, Ord )

mkB0 :: B0
mkB0 = B0 empty

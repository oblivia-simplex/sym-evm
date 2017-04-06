module SymEVM.Data.EVM.State.Env.Code where

import qualified Data.Vector as V
import Data.Word

import Text.Read
import Data.List.Split

type Code = V.Vector Word8

deserialize :: String -> Maybe Code
deserialize str =
  if strLen `mod` 2 == 0 then
    do
      let strBytes = map ("0x" ++) $ chunksOf 2 str
      words <- (mapM readMaybe strBytes :: Maybe [Word8])
      return (V.fromList words)
  else
    Nothing
  where
    strLen = length str

module SymEVM.Data.EVM.State.Env.Code where

import Data.Array
import Data.Word

import Text.Read
import Data.List.Split

type Code = Array Integer Word8

deserialize :: String -> Maybe Code
deserialize str =
  if strLen `mod` 2 == 0 then
    do
      let strBytes = map ("0x" ++) $ chunksOf 2 str
      words <- (mapM readMaybe strBytes :: Maybe [Word8])
      return (listArray (0, ((toInteger . length) words) - 1) words)
  else
    Nothing
  where
    strLen = length str

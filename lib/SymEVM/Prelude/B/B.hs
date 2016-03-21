module SymEVM.Prelude.B.B (B, toB, strToB, fromB, strToBS) where

import Data.Word
import Text.Read

import qualified Data.ByteString as BS
import Data.List.Split

import SymEVM.Prelude.Error

newtype B = B { fromB :: BS.ByteString } deriving ( Show, Eq, Ord )

toB :: BS.ByteString -> Error B
toB bs = Right $ B { fromB = bs }

strToB :: String -> Error B
strToB s =
    case bs of
        Nothing -> Left  $ "B: Failed to parse " ++ s
        Just v  -> toB v
    where
        bs = strToBS s

strToBS :: String -> Maybe BS.ByteString
strToBS s =
    (mapM (readMaybe . ("0x" ++)) $ chunksOf 2 s' :: Maybe [Word8]) >>= return . BS.pack
    where
        s' = if take 2 s == "0x" then drop 2 s else s

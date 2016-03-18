module SymEVM.Prelude.ByteArray 
    ( strToByteString
    , B20
    , toB20
    , strToB20
    , fromB20
    , B0
    , mkB0) where

import Prelude hiding (length)

import Data.ByteString
import Text.Read

-- TODO
readMaybe' :: String -> Maybe ByteString
readMaybe' s = 
    Nothing

strToByteString :: String -> Either String ByteString
strToByteString s =
    case bs of
        Nothing -> Left $ "ByteString: Failed to parse " ++ s
        Just v  -> Right v
    where
        bs = readMaybe' s

newtype B20 = B20 { fromB20 :: ByteString } deriving ( Show, Eq, Ord )

toB20 :: ByteString -> Either String B20
toB20 bs =
    if length bs == 20 then
        Right B20 { fromB20 = bs }
    else
        Left $ "B20: " ++ (show bs) ++ " not length 20"

strToB20 :: String -> Either String B20
strToB20 s =
    case bs of
        Nothing -> Left $ "B20: Failed to parse " ++ s
        Just v  -> toB20 v
    where
        bs = readMaybe' s

newtype B0 = B0 ByteString deriving ( Show, Eq, Ord )

mkB0 :: B0
mkB0 = B0 empty

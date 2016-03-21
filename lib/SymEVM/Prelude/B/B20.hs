module SymEVM.Prelude.B.B20 (B20, toB20, strToB20, fromB20) where

import Data.ByteString as BS
import Text.Read

import SymEVM.Prelude.Error
import SymEVM.Prelude.B.B

newtype B20 = B20 { fromB20 :: B } deriving ( Show, Eq, Ord )

toB20 :: BS.ByteString -> Error B20
toB20 bs =
    case bs' of
        Left err  -> Left err
        Right v ->
            if BS.length bs == 20 then
                Right $ B20 { fromB20 = v }
            else
                Left  $ "B20: " ++ (show bs) ++ " not length 20"
    where
        bs' = toB bs

strToB20 :: String -> Error B20
strToB20 s =
    case bs of
        Nothing -> Left $ "B20: Failed to parse " ++ s
        Just v  -> toB20 v
    where
        bs = strToBS s


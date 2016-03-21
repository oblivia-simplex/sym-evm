module SymEVM.Prelude.P.P256 (P256, toP256, strToP256, fromP256) where

import Text.Read

import SymEVM.Prelude.Error
import SymEVM.Prelude.P.P

newtype P256 = P256 { fromP256 :: P } deriving ( Show, Eq, Ord )

toP256 :: Integer -> Error P256
toP256 n =
    case n' of
        Left err   -> Left err
        Right v  ->
            if n < 2 ^ 256 then
                Right $ P256 { fromP256 = v }
            else
                Left  $ "P256: " ++ (show n) ++ " not in (0, 2^256)"
    where
        n' = toP n

strToP256 :: String -> Error P256
strToP256 s =
    case n of
        Nothing -> Left $ "P256: Failed to parse " ++ s
        Just v  -> toP256 v
    where
        n = readMaybe s :: Maybe Integer

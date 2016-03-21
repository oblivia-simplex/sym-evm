module SymEVM.Prelude.P.P (P, toP, strToP, fromP) where

import Text.Read

import SymEVM.Prelude.Error

newtype P = P { fromP :: Integer } deriving ( Show, Eq, Ord )

toP :: Integer -> Error P
toP n =
    if n > 0 then
        Right $ P { fromP = n }
    else
        Left  $ "P: " ++ (show n) ++ " not in (0, ∞)"

strToP :: String -> Error P
strToP s =
    case n of
        Nothing -> Left  $ "P: Failed to parse " ++ s
        Just v  -> toP v
    where
        n = readMaybe s :: Maybe Integer

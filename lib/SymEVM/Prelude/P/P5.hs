module SymEVM.Prelude.P.P5 (P5, toP5, strToP5, fromP5) where

import Text.Read

import SymEVM.Prelude.Error
import SymEVM.Prelude.P.P

newtype P5 = P5 { fromP5 :: P } deriving ( Show, Eq, Ord )

toP5 :: Integer -> Error P5
toP5 n =
    case n' of
        Left err  -> Left err
        Right v ->
            if n < 2 ^ 5 then
                Right $ P5 { fromP5 = v }
            else
                Left  $ "P5: " ++ (show n) ++ " not in (0, 2^5)"
    where
        n' = toP n

strToP5 :: String -> Error P5
strToP5 s =
    case n of
        Nothing -> Left $ "P5: Failed to parse " ++ s
        Just v  -> toP5 v
    where
        n = readMaybe s :: Maybe Integer

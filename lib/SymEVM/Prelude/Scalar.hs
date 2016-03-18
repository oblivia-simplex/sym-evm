module SymEVM.Prelude.Scalar
    ( Scalar
    , toScalar
    , fromScalar
    , S256
    , toS256
    , strToS256
    , fromS256
    , S5
    , toS5
    , fromS5
    ) where

import Text.Read

newtype Scalar = Scalar { fromScalar :: Integer } deriving (Show, Eq, Ord)

toScalar :: Integer -> Maybe Scalar
toScalar n =
    if n > 0 then
        Just Scalar { fromScalar = n }
    else
        Nothing

newtype S256 = S256 { fromS256 :: Integer } deriving (Show, Eq, Ord)

toS256 :: Integer -> Either String S256
toS256 n = 
    if 0 < n && n < 2 ^ 256 then 
        Right S256 { fromS256 = n }
    else 
        Left $ "S256: " ++ (show n) ++ " not in (0, 2^256)"

strToS256 :: String -> Either String S256
strToS256 s =
    case n of
        Nothing -> Left $ "S256: Failed to parse " ++ s
        Just v  -> toS256 v
    where
        n = readMaybe s :: Maybe Integer

newtype S5 = S5 { fromS5 :: Integer } deriving (Show, Eq, Ord)

toS5 :: Integer -> Maybe S5
toS5 n =
    if 0 < n && n < 2 ^ 5 then
        Just S5 { fromS5 = n }
    else
        Nothing

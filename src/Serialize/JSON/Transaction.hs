{-# LANGUAGE OverloadedStrings #-}

module Serialize.JSON.Transaction (TransactionData) where

{- TODO:
 -   + The Read instance for ByteString doesn't work how I want it to,
 -      need to parse them some other way
-}

import SymEVM.Prelude
import Data.ByteString
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types

data TransactionData = TransactionData
    { {- from :: B20
    , to :: Either B0 B20
    , -} gas :: S256
    , gasPrice :: S256
    } deriving ((Show))

strParse
    :: (Read a)
    => (a -> Maybe b) -> String -> String -> Parser b
strParse f err s = 
    case (f . read) s of
        Nothing -> fail $ "Expected " ++ err ++ " but got " ++ s
        Just val -> return val

strParseDefault
    :: (Read a)
    => (a -> Maybe c) -> b -> String -> Maybe String -> Parser (Either b c)
strParseDefault f def err s =
    case s of
        Nothing -> return $ Left def
        Just val -> strParse f err val >>= return. Right

  -- TODO: Pick reasonable default values
instance FromJSON TransactionData where
    parseJSON (Object v) = do
         {-fromV <- v .: "from" >>= strParse toB20 "address (from)"-}
         {-toV <- v .:? "to" >>= strParseDefault toB20 mkB0 "address (to)"-}
         gasV <- v .:? "gas" .!= "0x1000" >>= strParse toS256 "(0, 2^256) (gas)"
         gasPriceV <- v .:? "gasPrice" .!= "0x1000" >>= strParse toS256 "(0, 2^256) (gasPrice)"
         return TransactionData { {- from = fromV, to = toV, -} gas = gasV, gasPrice = gasPriceV }
 
    parseJSON invalid = typeMismatch "TransactionData" invalid

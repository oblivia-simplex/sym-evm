{-# LANGUAGE OverloadedStrings #-}

module Serialize.JSON.Transaction (deserialize) where

import Data.Maybe

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy

import SymEVM.Prelude
import qualified SymEVM.Data.Transaction as T

deserialize :: ByteString -> Error T.Transaction
deserialize bs =
    case bs' of
        Left err  -> Left err
        Right val -> Right $ convert val
    where
        bs' = eitherDecode bs :: Either String TransactionData


{- TODO:
 -   + nonce; comes from world state
 -   + ecSign, ecR, ecS; comes from signing raw transaction
 -
 - Need to create a monad for configuration (will provide elliptic curve
 - private key from user, and flags) and a monad for world state (will provide
 - lookup for nonce).
 -
 - Should also validate the 'from' field against the private keys in config monad,
 - and then use it to lookup nonce.
 -}
convert :: TransactionData -> T.Transaction
convert td =
    T.Transaction { T.nonce    = fromRight $ toP256 1
                  , T.gasPrice = (gasPrice td)
                  , T.gasLimit = (gas td)
                  , T.to       = (to td)
                  , T.value    = (value td)
                  , T.payload  = (payload td)
                  , T.ecSign   = fromRight $ toP5   1
                  , T.ecR      = fromRight $ toP256 1
                  , T.ecS      = fromRight $ toP256 1
                  }
    where
        fromRight (Right v) = v
        fromRight (Left err) = error err

data TransactionData = TransactionData
    { from :: B20
    , to :: Either B0 B20
    , gas :: P256
    , gasPrice :: P256
    , value :: P256
    , payload :: B
    } deriving ( Show )

-- TODO: Make defaults sensible
strDefaultGas :: String
strDefaultGas = "0x01"

strDefaultGasPrice :: String
strDefaultGasPrice = "0x01"

strDefaultValue :: String
strDefaultValue = "0x01"

errMsg :: String -> String -> String
errMsg field reason =
    "Failure while processing the \'" ++ field ++ "\' field. Reason:\n" ++ "  " ++ reason

strParse :: (String -> Either String a) -> String -> String -> Parser a
strParse p field s =
    case p s of
        Left why -> fail $ errMsg field why
        Right val -> return val

strToFrom :: String -> Parser B20
strToFrom = strParse strToB20 "from"

strToTo :: Maybe String -> Parser (Either B0 B20)
strToTo s =
    case s of
        Nothing -> return $ Left mkB0
        Just s' -> strParse strToB20 "to" s' >>= return . Right

strToGas :: String -> Parser P256
strToGas = strParse strToP256 "gas"

strToGasPrice :: String -> Parser P256
strToGasPrice = strParse strToP256 "gasPrice"

strToValue :: String -> Parser P256
strToValue = strParse strToP256 "value"

strToPayload :: String -> Parser B
strToPayload = strParse strToB "payload"

instance FromJSON TransactionData where
    parseJSON (Object v) = do
        fromV     <- v .:  "from"                            >>= strToFrom
        toV       <- v .:? "to"                              >>= strToTo
        gasV      <- v .:? "gas"      .!= strDefaultGas      >>= strToGas
        gasPriceV <- v .:? "gasPrice" .!= strDefaultGasPrice >>= strToGasPrice
        valueV    <- v .:? "value"    .!= strDefaultValue    >>= strToValue
        payloadV  <- v .:  "payload"                         >>= strToPayload

        return TransactionData { from     = fromV
                               , to       = toV
                               , gas      = gasV
                               , gasPrice = gasPriceV
                               , value    = valueV
                               , payload  = payloadV
                               }

    parseJSON invalid = typeMismatch "Transaction" invalid

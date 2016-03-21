{-# LANGUAGE OverloadedStrings #-}

module Serialize.JSON.Transaction (TransactionData) where

import SymEVM.Prelude
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types

data TransactionData = TransactionData
    { from :: B20
    , to :: Either B0 B20
    , gas :: P256
    , gasPrice :: P256
    , value :: P256
    , payload :: B
    } deriving ((Show))

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

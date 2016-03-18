module Main (main) where

import UI.CL
import Serialize.JSON.Transaction

import SymEVM.Data.Transaction

import Prelude hiding (readFile)
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import Data.Aeson

main :: IO ()
main = do
    (options, args) <- parseCL
    print options
    res <- readFile (args !! 0) >>= return . eitherDecode
    case (res :: Either String TransactionData) of
        Left err -> putStrLn err
        Right val -> print val

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson

import SymEVM.Prelude
import UI.CL
import SymEVM.Data.Transaction
import Serialize.JSON.Transaction

main :: IO ()
main = do
    (options, args) <- parseCL
    print options
    res <- L.readFile (args !! 0) >>= return . deserialize
    case (res :: Error Transaction) of
        Left err -> putStrLn err
        Right val -> print val

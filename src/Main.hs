module Main (main) where

import UI.CL

import SymEVM.Data.Transaction

main :: IO ()
main = do
    (options, args) <- parseCL
    putStrLn ""

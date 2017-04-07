module Main (main) where

import UI.CL

import SymEVM.Analysis.EVM

import SymEVM.Data.EVM.State.Env.Code

main :: IO ()
main = do
    (options, args) <- parseCL
    rawCode <- readFile (args !! 0)
    let rawCode' = drop 2 (take ((length rawCode) - 1) rawCode) -- remove leading 0x, and newline
    let code = deserialize rawCode'
    case code of
      Nothing -> putStrLn "Error processing bytecode"
      Just c  -> do print code
                    print (eval c)

module Main (main) where

import UI.CL

main :: IO ()
main = do
    (options, args) <- parseCL
    putStrLn ""

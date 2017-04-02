module UI.CL (parseCL, Options(..)) where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt

{- # Adding a Flag
 -
 - 1. Add a field to the Options record
 - 2. Add a default value for that field in defaultOptions record
 - 3. Add an OptDescr for the new option in the options value
 -
 - See: https://wiki.haskell.org/High-level_option_handling_with_GetOpt#About_typical_use_of_GetOpt
 -
 - For an example of how to add flags. This module is directly based on the above.
 -}

version :: String
version = "0.01"

header :: IO String
header = do
    prg <- getProgName
    return $ "Usage: " ++ prg ++ " [OPTIONS...]"

data Options = Options { optOpcodeInput :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { optOpcodeInput = False
                         }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "V" ["version"] 
        (NoArg (\_ -> do
                        hPutStrLn stderr ("Version: " ++ (show version))
                        exitSuccess))
        "Print version"
    , Option "h" ["help"]
        (NoArg (\_ -> do
                        h <- header
                        hPutStrLn stderr (usageInfo h options)
                        exitSuccess))
        "Show help"
    , Option "O" ["opcode"]
        (NoArg (\o -> do
                        return o { optOpcodeInput = True }))
        "Input is in opcode format"
    ]

parseCL :: IO (Options, [String])
parseCL = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    case errors of
        []   -> do
                    opts <- foldl (>>=) (return defaultOptions) actions
                    return (opts, nonOptions)
        errs -> do
                    h <- header
                    hPutStrLn stderr (concat errs ++ usageInfo h options)
                    exitFailure

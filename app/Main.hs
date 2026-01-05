module Main (main) where

import Scan
import LoxError
import System.IO
import System.IO.Error
import System.Environment
import Data.Char
import Control.Exception

run :: String -> IO ()
run s = case run' s of
            Right out -> print out
            Left err     -> reportErr err
        where run' = scan

runPrompt :: IO ()
runPrompt = do
    putStr ">"
    hFlush stdout
    l <- catch getLine (\e -> if isEOFError e then return "" else throwIO e)
    if all isSpace l
        then return ()
        else do run l
                runPrompt

runFile :: String -> IO ()
runFile s = do
    file <- readFile s
    run file

printUsage :: IO ()
printUsage = putStrLn "Usage: hlox [script]"

runArgs :: [String] -> IO ()
runArgs []  = runPrompt
runArgs [x] = runFile x
runArgs _   = printUsage

main :: IO ()
main = do args <- getArgs
          runArgs args

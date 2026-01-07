module Main (main) where

import Scan
import Parse
import Interpret
import LoxError
import System.IO
import System.IO.Error
import System.Environment
import Data.Char
import Control.Exception

runScanParse :: String -> Either LoxError [Stmt]
runScanParse s = do
    tokens <- scan s
    parse tokens

run :: String -> IO ()
-- Scanning and parsing: pure phase
run s = case runScanParse s of
    Left err -> reportErr err
    Right stmts -> do
        -- Interpreting: effectful phase (with IO)
        possibleErr <- interpret stmts
        case possibleErr of
            Left err -> reportErr err
            Right _  -> return ()

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

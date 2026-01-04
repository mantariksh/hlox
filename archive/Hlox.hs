import Scanner
import System.Environment
import System.IO

runPrompt :: IO ()
runPrompt = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if null line
    then return ()
    else do
      print (scanTokens line)
      runPrompt

runFile :: String -> IO ()
runFile filename = do
  contents <- readFile filename
  print (scanTokens contents)

runHlox :: [String] -> IO ()
runHlox args
  | length args > 1 = putStrLn "Usage: hlox [script]"
  | length args == 1 = (runFile . head) args
  | otherwise = runPrompt

main = do
  args <- getArgs
  runHlox args

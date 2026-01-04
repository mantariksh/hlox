module Scanner (scanTokens) where

import Data.Char
import LoxError
import Token (Literal (..), Token (..))
import TokenType (TokenType (..))

isAllDigits :: String -> Bool
isAllDigits = all isDigit

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\r', '\t', '\n', '\f', '\v']

data ScanState = ScanState {currLexeme :: String, currTokens :: [Token], currLine :: Int} deriving (Show)

initialScanState :: ScanState
initialScanState = ScanState {currLexeme = "", currTokens = [], currLine = 1}

initialiseToken :: String -> Int -> Token
initialiseToken s line
  | isAllDigits s = Token {tokenType = NUMBER, lexeme = s, literal = IntegerLit sAsNum, Token.line = line}
  where
    sAsNum = read s :: Int

addLexemeToTokens :: String -> Int -> [Token] -> [Token]
addLexemeToTokens lexeme line tokens = tokens ++ [initialiseToken lexeme line]

scanTokensAcc :: Either ScanError ScanState -> Char -> Either ScanError ScanState
scanTokensAcc (Left err) c = Left err
scanTokensAcc (Right (ScanState {currLexeme = cLexeme, currTokens = cTokens, currLine = cLine})) c
  | isWhitespace c && (not . null) cLexeme = Right (ScanState {currLexeme = "", currTokens = addLexemeToTokens cLexeme cLine cTokens, currLine = cLine})
  | isDigit c = Right (ScanState {currLexeme = cLexeme ++ [c], currTokens = cTokens, currLine = cLine})

scanTokens :: String -> Either ScanError [Token]
scanTokens input =
  case foldl scanTokensAcc (Right initialScanState) input of
    Left err -> Left err
    Right (ScanState {currLexeme = cLexeme, currTokens = cTokens, currLine = cLine}) -> if null cLexeme then Right cTokens else Right (cTokens ++ [initialiseToken cLexeme cLine])

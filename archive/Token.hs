module Token (Token (..), Literal (..)) where

import TokenType (TokenType)

data Literal = StringLit String | IntegerLit Int | DoubleLit Double | NilLit deriving (Show)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    literal :: Literal,
    line :: Int
  }
  deriving (Show)
module Token
( TokenType(..)
, Token(..)
) where

data TokenType =
    -- Single-character tokens
      LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    -- One or two character tokens
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    -- Literals
    | Identifier String
    | String String
    | Number Double
    -- Keywords
    | And
    | Class
    | Else
    | FalseToken
    | Fun
    | For
    | If 
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | TrueToken
    | Var
    | While
    -- End of file token
    | EOF
    deriving (Eq)

instance Show TokenType where
    show LeftParen = "("
    show RightParen = ")"
    show LeftBrace = "{"
    show RightBrace = "}"
    show Comma = ","
    show Dot = "."
    show Minus = "-"
    show Plus = "+"
    show Semicolon = ";"
    show Slash = "/"
    show Star = "*"
    show Bang = "!"
    show BangEqual = "!="
    show Equal = "="
    show EqualEqual = "=="
    show Greater = ">"
    show GreaterEqual = ">="
    show Less = "<"
    show LessEqual = "<="
    show (Identifier s) = s
    show (String s) = s
    show (Number x) = show x
    show And = "and"
    show Class = "class"
    show Else = "else"
    show FalseToken = "false"
    show Fun = "fun"
    show For = "for"
    show If = "if"
    show Nil = "nil"
    show Or = "or"
    show Print = "print"
    show Return = "return"
    show Super = "super"
    show This = "this"
    show TrueToken = "true"
    show Var = "var"
    show While = "while"
    show EOF = "EOF"

type LineNo = Int
data Token = Token TokenType LineNo
    deriving (Eq)

instance Show Token where
    show (Token t l) = show t ++ " " ++ show l

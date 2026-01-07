module ExprOut
( ExprOut(..)
) where

data ExprOut =
    StrOut String
  | NumOut Double
  | BoolOut Bool
  | NilOut
  deriving (Eq)

instance Show ExprOut where
    show (StrOut s) = s
    show (NumOut n) = show n
    show (BoolOut b) = if b then "true" else "false"
    show NilOut = "nil"

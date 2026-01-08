module ExprOut
( ExprOut(..)
) where

import Stmt

data ExprOut =
    StrOut String
  | NumOut Double
  | BoolOut Bool
  | NilOut
  -- A function is a valid value produced by an expression.
  -- Original identifier, params, body
  | FunOut String [String] Stmt
  deriving (Eq)

instance Show ExprOut where
    show (StrOut s) = s
    show (NumOut n) = show n
    show (BoolOut b) = if b then "true" else "false"
    show NilOut = "nil"
    show (FunOut funName _ _) = "<fn " ++ funName ++ ">"

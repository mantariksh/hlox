module Expr
( Expr(..)
) where

import Token

-- We store underlying tokens where possible so we can
-- subsequently report the line number of the operation if
-- there are evaluation errors
data Expr =
    LitNum Double
  | LitStr String
  | LitBool Bool
  | LitNil
  | Grouping Expr
  | Unary Token Expr
  | Binary Expr Token Expr
  -- We keep the token to track the line number in case of
  -- exceptions, and duplicate the string to avoid unnecessary
  -- pattern matching on the token type.
  | Variable Token String
  | Assign Expr Expr
  | Logical Expr Token Expr
  -- Callee, closing paren, arguments
  | Call Expr Token [Expr]
  deriving (Eq)

showArgs :: [Expr] -> String
showArgs [] = ""
showArgs [x] = show x
showArgs (x:xs) = show x ++ ", " ++ showArgs xs

instance Show Expr where
    show (LitNum n) = show n
    show (LitStr s) = s
    show (LitBool b) = if b then "true" else "false"
    show (LitNil) = "nil"
    show (Grouping e) = "(group " ++ show e ++ ")"
    show (Unary (Token op _) e) = '(':show op ++ show e ++ ")"
    show (Binary e1 (Token op _) e2) = '(':show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Variable _ s) = s
    show (Assign lhs rhs) = show lhs ++ " = " ++ show rhs
    show (Logical e1 op e2) = '(':show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Call callee _ args) =
        show callee ++ "(" ++ showArgs args ++ ")"

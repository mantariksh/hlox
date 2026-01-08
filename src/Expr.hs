module Expr
( Expr(..)
) where

import Token

-- We store underlying tokens where possible so we can
-- subsequently report the line number of the operation if
-- there are evaluation errors
data Expr =
    Literal Token
  | Grouping Expr
  | Unary Token Expr
  | Binary Expr Token Expr
  | Variable Token
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
    show (Literal (Token t _)) = show t
    show (Grouping e) = "(group " ++ show e ++ ")"
    show (Unary (Token op _) e) = '(':show op ++ show e ++ ")"
    show (Binary e1 (Token op _) e2) = '(':show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Variable (Token s _)) = show s
    show (Assign lhs rhs) = show lhs ++ " = " ++ show rhs
    show (Logical e1 op e2) = '(':show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Call callee _ args) =
        show callee ++ "(" ++ showArgs args ++ ")"

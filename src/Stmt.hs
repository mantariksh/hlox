module Stmt
( Stmt(..)
) where

import Expr
import Token

data Stmt =
    ExprStmt Expr
  | PrintStmt Expr
  | VarStmt Token Expr
  | VarStmtNoInit Token
  | Block [Stmt]
  | IfThenElse Expr Stmt Stmt
  | IfThen Expr Stmt
  --   Note that for loops desugar to WhileStmt
  | WhileStmt Expr Stmt
  --   Identifier, params, body
  | FunStmt Token [Token] Stmt
  --   Return keyword and expression
  | ReturnStmt Token Expr
  deriving (Eq, Show)

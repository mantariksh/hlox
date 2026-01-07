module Interpret
( interpret
) where

import Parse
import Token
import LoxError
import Environment
import ExprOut

handleBinary :: Token -> ExprOut -> ExprOut -> Either LoxError ExprOut
handleBinary op@(Token Plus _) e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ NumOut (n1 + n2)
    (NumOut _, _)          -> Left (makeTokenErr op "Operand must be a number.")
    (StrOut s1, StrOut s2) -> return $ StrOut (s1 ++ s2)
    (StrOut _, _)          -> Left (makeTokenErr op "Operand must be a string.")
    _ -> Left (makeTokenErr op "Operand must be a number or string.")

handleBinary op@(Token Minus _) e1 e2 = binaryNumNumHelper (-) op e1 e2
handleBinary op@(Token Star _) e1 e2 = binaryNumNumHelper (*) op e1 e2

handleBinary op@(Token Slash _) e1 e2 = case e2 of
    NumOut 0 -> Left (makeTokenErr op "Cannot divide by zero.")
    _        -> binaryNumNumHelper (/) op e1 e2

handleBinary (Token EqualEqual _) e1 e2 = return $ BoolOut (e1 == e2)
handleBinary (Token BangEqual _) e1 e2 = return $ BoolOut (e1 /= e2)

handleBinary op@(Token Greater _) e1 e2 = binaryNumBoolHelper (>) op e1 e2
handleBinary op@(Token GreaterEqual _) e1 e2 = binaryNumBoolHelper (>=) op e1 e2
handleBinary op@(Token Less _) e1 e2 = binaryNumBoolHelper (<) op e1 e2
handleBinary op@(Token LessEqual _) e1 e2 = binaryNumBoolHelper (<=) op e1 e2

handleBinary t _ _ = Left (makeTokenErr t "Unexpected token.")

binaryNumNumHelper :: (Double -> Double -> Double) -> Token -> ExprOut -> ExprOut -> Either LoxError ExprOut
binaryNumNumHelper f op e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ NumOut (f n1 n2)
    _ -> Left (makeTokenErr op "Operand must be a number.")

binaryNumBoolHelper :: (Double -> Double -> Bool) -> Token -> ExprOut -> ExprOut -> Either LoxError ExprOut
binaryNumBoolHelper f op e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ BoolOut (f n1 n2)
    _ -> Left (makeTokenErr op "Operand must be a number.")

handleUnary :: Token -> ExprOut -> Either LoxError ExprOut
handleUnary op@(Token Minus _) e = case e of
    NumOut n -> return $ NumOut (-n)
    _        -> Left (makeTokenErr op "Expected number for negation.")

-- As per spec, false and nil are falsey, everything else is truthy
handleUnary (Token Bang _) e = case e of
    BoolOut False -> return $ BoolOut True
    NilOut        -> return $ BoolOut True
    _             -> return $ BoolOut False

handleUnary token _ = Left (makeTokenErr token "Unexpected token.")

handleVar :: Token -> Environment -> Either LoxError ExprOut
handleVar t@(Token (Identifier _) _) env = getVar env t
handleVar t _ = Left (makeTokenErr t "Expected identifier.")

handleAssign :: Expr -> ExprOut -> Environment -> Either LoxError Environment
handleAssign lhs rhs env =
    case lhs of
        Variable t -> assignVar env t rhs
        _ -> Left (LoxError 1 "" "Invalid assignment target.")

litToOut :: Expr -> Either LoxError ExprOut
litToOut (Literal lit@(Token t _)) = case t of
    Number n -> return $ NumOut n
    String s -> return $ StrOut s
    TrueToken -> return $ BoolOut True
    FalseToken -> return $ BoolOut False
    Nil -> return $ NilOut
    _ -> Left (makeTokenErr lit "Unexpected literal token.")
litToOut _ = Left (LoxError 1 "" "Expected literal.")

evaluate :: Expr -> Environment -> Either LoxError (ExprOut, Environment)
evaluate expr env = case expr of
    Literal l -> do
        out <- litToOut (Literal l)
        return (out, env)
    Grouping e -> evaluate e env
    Unary op e -> do
        (outR, env') <- evaluate e env
        out <- handleUnary op outR
        return (out, env')
    Binary e1 op e2 -> do
        (outL, env') <- evaluate e1 env
        (outR, env'') <- evaluate e2 env'
        out <- handleBinary op outL outR
        return (out, env'')
    Variable v -> do
        out <- handleVar v env
        return (out, env)
    Assign lhs rhs -> do
        (out, env') <- evaluate rhs env
        -- The LHS looks like an Expr but doesn't evaluate
        -- to a value. In technical terms, the LHS is an l-value.
        -- e.g. obj.get().prop = 1;
        env'' <- handleAssign lhs out env'
        return (out, env'')

interpret' :: [Stmt] -> Environment -> Either LoxError (IO ())
interpret' [] _ = Right (return ())

interpret' ((ExprStmt e):stmts') env = do
    (_, env') <- evaluate e env
    interpret' stmts' env'

interpret' (PrintStmt e:stmts') env = do
    (e', env') <- evaluate e env
    rest <- interpret' stmts' env'
    return (print e' >> rest)

interpret' ((VarStmt (Token (Identifier s) _) e):stmts') env = do
    (rhs, env') <- evaluate e env
    interpret' stmts' (define env' s rhs)
interpret' ((VarStmt t _):_) _ = Left (makeTokenErr t "Expected identifier.")

interpret' (VarStmtNoInit (Token (Identifier s) _):stmts') env =
    interpret' stmts' (define env s NilOut)
interpret' ((VarStmtNoInit t):_) _ = Left (makeTokenErr t "Expected identifier.")

interpret :: [Stmt] -> Either LoxError (IO ())
interpret stmts = interpret' stmts emptyEnv

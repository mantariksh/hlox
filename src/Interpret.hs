module Interpret
( interpret
) where

import Parse
import Token
import LoxError
import Environment

handleBinary :: Token -> Expr -> Expr -> Environment -> Either LoxError (Expr, Environment)
-- Need to treat + specially because it applies to both numbers and strings
handleBinary op@(Token Plus _) e1 e2 env = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number n2) _) ->
            return (Literal (Token (Number (n1 + n2)) ln1), env)
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            (e2', env') <- evaluate e2 env
            handleBinary op e1 e2' env'
    Literal (Token (String s1) ln1) -> case e2 of
        Literal (Token (String s2) _) ->
            return (Literal (Token (String (s1 ++ s2)) ln1), env)
        Literal _ ->
            Left (makeTokenErr op "Operand must be a string.")
        _ -> do
            (e2', env') <- evaluate e2 env
            handleBinary op e1 e2' env'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number or string.")
    _ -> do
        (e1', env') <- evaluate e1 env
        handleBinary op e1' e2 env'

handleBinary op@(Token Minus _) e1 e2 env = binaryNumNumHelper (-) op e1 e2 env

handleBinary op@(Token Star _) e1 e2 env = binaryNumNumHelper (*) op e1 e2 env

-- Need to treat / specially because of possible division by zero
handleBinary op@(Token Slash _) e1 e2 env = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number 0) _) ->
            Left (makeTokenErr op "Cannot divide by zero.")
        Literal (Token (Number n2) _) ->
            return (Literal (Token (Number (n1 / n2)) ln1), env)
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            (e2', env') <- evaluate e2 env
            handleBinary op e1 e2' env'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number.")
    _ -> do
        (e1', env') <- evaluate e1 env
        handleBinary op e1' e2 env'

handleBinary op@(Token EqualEqual _) e1 e2 env =
    case e1 of
        Literal (Token t1 ln1) -> case e2 of
            Literal (Token t2 _) ->
                return (Literal (Token (if t1 == t2 then TrueToken else FalseToken) ln1), env)
            _ -> do
                (e2', env') <- evaluate e2 env
                handleBinary op e1 e2' env'
        _ -> do
            (e1', env') <- evaluate e1 env
            handleBinary op e1' e2 env'

handleBinary op@(Token BangEqual _) e1 e2 env =
    case e1 of
        Literal (Token t1 ln1) -> case e2 of
            Literal (Token t2 _) ->
                return (Literal (Token (if t1 /= t2 then TrueToken else FalseToken) ln1), env)
            _ -> do
                (e2', env') <- evaluate e2 env
                handleBinary op e1 e2' env'
        _ -> do
            (e1', env') <- evaluate e1 env
            handleBinary op e1' e2 env'

handleBinary op@(Token Greater _) e1 e2 env =
    binaryNumBoolHelper (>) op e1 e2 env
handleBinary op@(Token GreaterEqual _) e1 e2 env =
    binaryNumBoolHelper (>=) op e1 e2 env
handleBinary op@(Token Less _) e1 e2 env =
    binaryNumBoolHelper (<) op e1 e2 env
handleBinary op@(Token LessEqual _) e1 e2 env =
    binaryNumBoolHelper (<=) op e1 e2 env

handleBinary token _ _ _ = Left (makeTokenErr token "Unexpected token.")

binaryNumNumHelper :: (Double -> Double -> Double) -> Token -> Expr -> Expr -> Environment -> Either LoxError (Expr, Environment)
binaryNumNumHelper f op e1 e2 env = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number n2) _) ->
            return (Literal (Token (Number (f n1 n2)) ln1), env)
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            (e2', env') <- evaluate e2 env
            handleBinary op e1 e2' env'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number.")
    _ -> do
        (e1', env') <- evaluate e1 env
        handleBinary op e1' e2 env'

binaryNumBoolHelper :: (Double -> Double -> Bool) -> Token -> Expr -> Expr -> Environment -> Either LoxError (Expr, Environment)
binaryNumBoolHelper f op e1 e2 env = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number n2) _) ->
            return (Literal (Token (if f n1 n2 then TrueToken else FalseToken) ln1), env)
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            (e2', env') <- evaluate e2 env
            handleBinary op e1 e2' env'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number.")
    _ -> do
        (e1', env') <- evaluate e1 env
        handleBinary op e1' e2 env'

handleUnary :: Token -> Expr -> Environment -> Either LoxError (Expr, Environment)
handleUnary op@(Token Minus _) e env = case e of
    Literal (Token (Number n) ln) -> return (Literal (Token (Number (-n)) ln), env)
    Literal t -> Left (makeTokenErr t "Expected number for negation.")
    _ -> do
        (e', env') <- evaluate e env
        handleUnary op e' env'
-- As per spec, false and nil are falsey, everything else is truthy
handleUnary op@(Token Bang _) e env = case e of
    Literal (Token FalseToken ln) -> return (Literal (Token TrueToken ln), env)
    Literal (Token Nil ln) -> return (Literal (Token TrueToken ln), env)
    Literal (Token _ ln) -> return (Literal (Token FalseToken ln), env)
    _ -> do
        (e', env') <- evaluate e env
        handleUnary op e' env'
handleUnary token _ _ = Left (makeTokenErr token "Unexpected token.")

handleVar :: Token -> Environment -> Either LoxError (Expr, Environment)
handleVar t@(Token (Identifier _) _) env = do
    e <- getVar env t
    evaluate e env
handleVar t _ = Left (makeTokenErr t "Expected identifier.")

handleAssign :: Expr -> Expr -> Environment -> Either LoxError (Expr, Environment)
handleAssign lhs rhs env =
    case lhs of
        Variable t -> do
            (rhs', env') <- evaluate rhs env
            env'' <- assignVar env' t rhs'
            return (rhs', env'')
        _ -> Left (LoxError 1 "" "Invalid assignment target.")

-- The idea is to boil down an expression to a literal value
evaluate :: Expr -> Environment -> Either LoxError (Expr, Environment)
evaluate expr env = case expr of
    Literal l -> Right (Literal l, env)
    Grouping e -> evaluate e env
    Unary op e -> handleUnary op e env
    Binary e1 op e2 -> handleBinary op e1 e2 env
    Variable v -> handleVar v env
    Assign lhs rhs -> handleAssign lhs rhs env

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

interpret' (VarStmtNoInit (Token (Identifier s) ln):stmts') env =
    interpret' stmts' (define env s (Literal (Token Nil ln)))
interpret' ((VarStmtNoInit t):_) _ = Left (makeTokenErr t "Expected identifier.")

interpret :: [Stmt] -> Either LoxError (IO ())
interpret stmts = interpret' stmts emptyEnv

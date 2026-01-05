module Interpret
( interpret
) where

import Parse
import Token
import LoxError

handleBinary :: Token -> Expr -> Expr -> Either LoxError Expr
handleBinary op@(Token Plus _) e1 e2 = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number n2) _) ->
            return (Literal (Token (Number (n1 + n2)) ln1))
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            e2' <- evaluate e2
            handleBinary op e1 e2'
    Literal (Token (String s1) ln1) -> case e2 of
        Literal (Token (String s2) _) ->
            return (Literal (Token (String (s1 ++ s2)) ln1))
        Literal _ ->
            Left (makeTokenErr op "Operand must be a string.")
        _ -> do
            e2' <- evaluate e2
            handleBinary op e1 e2'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number or string.")
    _ -> do
        e1' <- evaluate e1
        handleBinary op e1' e2

handleBinary op@(Token Minus _) e1 e2 = binaryNumNumHelper (-) op e1 e2

handleBinary op@(Token Star _) e1 e2 = binaryNumNumHelper (*) op e1 e2

handleBinary op@(Token Slash _) e1 e2 = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number 0) _) ->
            Left (makeTokenErr op "Cannot divide by zero.")
        Literal (Token (Number n2) _) ->
            return (Literal (Token (Number (n1 / n2)) ln1))
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            e2' <- evaluate e2
            handleBinary op e1 e2'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number.")
    _ -> do
        e1' <- evaluate e1
        handleBinary op e1' e2

handleBinary op@(Token EqualEqual _) e1 e2 =
    case e1 of
        Literal (Token (Number n1) ln1) -> case e2 of
            Literal (Token (Number n2) _) ->
                return (Literal (Token (if n1 == n2 then TrueToken else FalseToken) ln1))
            Literal _ ->
                return (Literal (Token FalseToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token (String s1) ln1) -> case e2 of
            Literal (Token (String s2) _) ->
                return (Literal (Token (if s1 == s2 then TrueToken else FalseToken) ln1))
            Literal _ ->
                return (Literal (Token FalseToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token TrueToken ln1) -> case e2 of
            Literal (Token TrueToken _) ->
                return (Literal (Token TrueToken ln1))
            Literal _ ->
                return (Literal (Token FalseToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token FalseToken ln1) -> case e2 of
            Literal (Token FalseToken _) ->
                return (Literal (Token TrueToken ln1))
            Literal _ ->
                return (Literal (Token FalseToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token Nil ln1) -> case e2 of
            Literal (Token Nil _) ->
                return (Literal (Token TrueToken ln1))
            _ -> return (Literal (Token FalseToken ln1))
        _ -> do
            e1' <- evaluate e1
            handleBinary op e1' e2

handleBinary op@(Token BangEqual _) e1 e2 =
    case e1 of
        Literal (Token (Number n1) ln1) -> case e2 of
            Literal (Token (Number n2) _) ->
                return (Literal (Token (if n1 /= n2 then TrueToken else FalseToken) ln1))
            Literal _ ->
                return (Literal (Token FalseToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token (String s1) ln1) -> case e2 of
            Literal (Token (String s2) _) ->
                return (Literal (Token (if s1 /= s2 then TrueToken else FalseToken) ln1))
            Literal _ ->
                return (Literal (Token FalseToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token TrueToken ln1) -> case e2 of
            Literal (Token TrueToken _) ->
                return (Literal (Token FalseToken ln1))
            Literal _ ->
                return (Literal (Token TrueToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token FalseToken ln1) -> case e2 of
            Literal (Token FalseToken _) ->
                return (Literal (Token FalseToken ln1))
            Literal _ ->
                return (Literal (Token TrueToken ln1))
            _ -> do
                e2' <- evaluate e2
                handleBinary op e1 e2'
        Literal (Token Nil ln1) -> case e2 of
            Literal (Token Nil _) ->
                return (Literal (Token FalseToken ln1))
            _ -> return (Literal (Token TrueToken ln1))
        _ -> do
            e1' <- evaluate e1
            handleBinary op e1' e2

handleBinary op@(Token Greater _) e1 e2 =
    binaryNumBoolHelper (>) op e1 e2
handleBinary op@(Token GreaterEqual _) e1 e2 =
    binaryNumBoolHelper (>=) op e1 e2
handleBinary op@(Token Less _) e1 e2 =
    binaryNumBoolHelper (<) op e1 e2
handleBinary op@(Token LessEqual _) e1 e2 =
    binaryNumBoolHelper (<=) op e1 e2

handleBinary token _ _ = Left (makeTokenErr token "Unexpected token.")

binaryNumNumHelper :: (Double -> Double -> Double) -> Token -> Expr -> Expr -> Either LoxError Expr
binaryNumNumHelper f op e1 e2 = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number n2) _) ->
            return (Literal (Token (Number (f n1 n2)) ln1))
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            e2' <- evaluate e2
            handleBinary op e1 e2'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number.")
    _ -> do
        e1' <- evaluate e1
        handleBinary op e1' e2

binaryNumBoolHelper :: (Double -> Double -> Bool) -> Token -> Expr -> Expr -> Either LoxError Expr
binaryNumBoolHelper f op e1 e2 = case e1 of
    Literal (Token (Number n1) ln1) -> case e2 of
        Literal (Token (Number n2) _) ->
            return (Literal (Token (if f n1 n2 then TrueToken else FalseToken) ln1))
        Literal _ ->
            Left (makeTokenErr op "Operand must be a number.")
        _ -> do
            e2' <- evaluate e2
            handleBinary op e1 e2'
    Literal _ ->
        Left (makeTokenErr op "Operand must be a number.")
    _ -> do
        e1' <- evaluate e1
        handleBinary op e1' e2

handleUnary :: Token -> Expr -> Either LoxError Expr
handleUnary op@(Token Minus _) e = case e of
    Literal (Token (Number n) ln) -> return (Literal (Token (Number (-n)) ln))
    Literal t -> Left (makeTokenErr t "Expected number for negation.")
    _ -> do
        e' <- evaluate e
        handleUnary op e'
-- As per spec, false and nil are falsey, everything else is truthy
handleUnary op@(Token Bang _) e = case e of
    Literal (Token FalseToken ln) -> return (Literal (Token TrueToken ln))
    Literal (Token Nil ln) -> return (Literal (Token TrueToken ln))
    Literal (Token _ ln) -> return (Literal (Token FalseToken ln))
    _ -> do
        e' <- evaluate e
        handleUnary op e'
handleUnary token _ = Left (makeTokenErr token "Unexpected token.")

-- The idea is to boil down an expression to a literal value
evaluate :: Expr -> Either LoxError Expr
evaluate expr = case expr of
    Literal l -> Right (Literal l)
    Grouping e -> evaluate e
    Unary op e -> handleUnary op e
    Binary e1 op e2 -> handleBinary op e1 e2

interpret :: [Stmt] -> Either LoxError (IO ())
interpret stmts = go stmts
    where go [] = Right (return ())
          go ((ExprStmt e):stmts') = do
            _ <- evaluate e
            go stmts'
          go (PrintStmt e:stmts') = do
            r <- evaluate e
            rest <- go stmts'
            return (print r >> rest)

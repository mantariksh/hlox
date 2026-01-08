module Interpret
( interpret
) where

import Parse
import Token
import LoxError
import Environment
import ExprOut

import Control.Monad.State
import Control.Monad.Except

-- Model statement as a state change from the initial environment
-- to a tuple of (possible error, new environment), with IO side-effects.
-- Environment -> IO (Either LoxError (), Environment)
-- There may be both an error and a new environment if e.g. the
-- environment was modified before the error occurred. This can
-- enable synchronization (i.e. continuing to interpret in spite
-- of errors) in future.
-- 
type Interpret = ExceptT LoxError (StateT Environment IO)

-- Model expression as a state change from an initial environment
-- to another environment, outputting an Either LoxError ExprOut.
-- Environment -> (Either LoxError ExprOut, Environment) 
type Eval = ExceptT LoxError (State Environment)

-- Expression evaluation helpers

handleBinary :: Token -> ExprOut -> ExprOut -> Eval ExprOut
handleBinary op@(Token Plus _) e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ NumOut (n1 + n2)
    (NumOut _, _)          -> throwError (makeTokenErr op "Operand must be a number.")

    (StrOut s1, StrOut s2) -> return $ StrOut (s1 ++ s2)
    (StrOut _, _)          -> throwError (makeTokenErr op "Operand must be a string.")

    _                      -> throwError (makeTokenErr op "Operand must be a number or string.")

handleBinary op@(Token Minus _) e1 e2 = binaryNumNumHelper (-) op e1 e2
handleBinary op@(Token Star _) e1 e2 = binaryNumNumHelper (*) op e1 e2

handleBinary op@(Token Slash _) e1 e2 = case e2 of
    NumOut 0 -> throwError (makeTokenErr op "Cannot divide by zero.")
    _        -> binaryNumNumHelper (/) op e1 e2

handleBinary (Token EqualEqual _) e1 e2 = return $ BoolOut (e1 == e2)
handleBinary (Token BangEqual _) e1 e2 = return $ BoolOut (e1 /= e2)

handleBinary op@(Token Greater _) e1 e2 = binaryNumBoolHelper (>) op e1 e2
handleBinary op@(Token GreaterEqual _) e1 e2 = binaryNumBoolHelper (>=) op e1 e2
handleBinary op@(Token Less _) e1 e2 = binaryNumBoolHelper (<) op e1 e2
handleBinary op@(Token LessEqual _) e1 e2 = binaryNumBoolHelper (<=) op e1 e2

handleBinary t _ _ = throwError (makeTokenErr t "Unexpected token.")

binaryNumNumHelper :: (Double -> Double -> Double) -> Token -> ExprOut -> ExprOut -> Eval ExprOut
binaryNumNumHelper f op e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ NumOut (f n1 n2)
    _ -> throwError (makeTokenErr op "Operand must be a number.")

binaryNumBoolHelper :: (Double -> Double -> Bool) -> Token -> ExprOut -> ExprOut -> Eval ExprOut
binaryNumBoolHelper f op e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ BoolOut (f n1 n2)
    _ -> throwError (makeTokenErr op "Operand must be a number.")

-- As per spec, false and nil are falsey, everything else is truthy
isTruthy :: ExprOut -> Bool
isTruthy (BoolOut False) = False
isTruthy (NilOut)        = False
isTruthy _               = True

handleUnary :: Token -> ExprOut -> Eval ExprOut
handleUnary op@(Token Minus _) e = case e of
    NumOut n -> return $ NumOut (-n)
    _        -> throwError (makeTokenErr op "Expected number for negation.")

handleUnary (Token Bang _) e = return $ BoolOut ((not . isTruthy) e)

handleUnary token _ = throwError (makeTokenErr token "Unexpected token.")

handleVar :: Token -> Eval ExprOut
handleVar t@(Token (Identifier _) _) = do
    env <- get
    case getVar env t of
        Left err -> throwError err
        Right e  -> return e
handleVar t = throwError (makeTokenErr t "Expected identifier.")

handleAssign :: Expr -> ExprOut -> Eval ExprOut
handleAssign lhs rhs = do
    env <- get
    case lhs of
        Variable t -> do
            case assignVar env t rhs of
                Left err   -> throwError err
                Right env' -> put env'
            return rhs
        _ -> throwError (LoxError 1 "" "Invalid assignment target.")

handleLogical :: Token -> ExprOut -> ExprOut -> Eval ExprOut
handleLogical (Token And _) e1 e2 =
    return $ BoolOut $ isTruthy e1 && isTruthy e2

handleLogical (Token Or _) e1 e2 =
    return $ BoolOut $ isTruthy e1 || isTruthy e2

handleLogical t _ _ = throwError (makeTokenErr t "Unexpected token.")

litToOut :: Expr -> Eval ExprOut
litToOut (Literal lit@(Token t _)) = case t of
    Number n -> return $ NumOut n
    String s -> return $ StrOut s
    TrueToken -> return $ BoolOut True
    FalseToken -> return $ BoolOut False
    Nil -> return $ NilOut
    _ -> throwError (makeTokenErr lit "Unexpected literal token.")
litToOut _ = throwError (LoxError 1 "" "Expected literal.")

evaluate :: Expr -> Eval ExprOut
evaluate expr =
    case expr of
        lit@(Literal _) -> litToOut lit
        Grouping e -> evaluate e
        Unary op e -> do
            outR <- evaluate e
            handleUnary op outR
        Binary e1 op e2 -> do
            outL <- evaluate e1
            outR <- evaluate e2
            handleBinary op outL outR
        Variable v -> handleVar v
        Assign lhs rhs -> do
            out <- evaluate rhs
            -- The LHS looks like an Expr but doesn't evaluate
            -- to a value. In technical terms, the LHS is an l-value.
            -- e.g. obj.get().prop = 1;
            handleAssign lhs out
        Logical e1 op e2 -> do
            outL <- evaluate e1
            outR <- evaluate e2
            handleLogical op outL outR

-- Lift Eval a into Interpret a
liftEval :: Eval a -> Interpret a
liftEval eval = do
    env <- get
    let (result, env') = runState (runExceptT eval) env
    put env'
    case result of
        Left err -> throwError err
        Right exprOut -> return exprOut

evaluateM :: Expr -> Interpret ExprOut
evaluateM e = liftEval $ evaluate e

interpret' :: Stmt -> Interpret ()
interpret' (ExprStmt e) = do
    -- Throw away output of expression, e.g. 1 + 1;
    _ <- evaluateM e
    return ()

interpret' (PrintStmt e) = do
    out <- evaluateM e
    liftIO $ print out

interpret' (VarStmt (Token (Identifier s) _) e) = do
    rhs <- evaluateM e
    env <- get
    put (define env s rhs)

interpret' (VarStmt t _) = throwError (makeTokenErr t "Expected identifier.")

interpret' (VarStmtNoInit (Token (Identifier s) _)) = do
    env <- get
    put (define env s NilOut)

interpret' (VarStmtNoInit t) = throwError (makeTokenErr t "Expected identifier.")

interpret' (Block stmts) = do
    env <- get
    put (pushCtx env)
    mapM_ interpret' stmts
    env' <- get
    put (popCtx env')

interpret' (IfThenElse cond thenStmt elseStmt) = do
    condOut <- evaluateM cond
    if isTruthy condOut
        then interpret' thenStmt
        else interpret' elseStmt

interpret' (IfThen cond thenStmt) = do
    condOut <- evaluateM cond
    if isTruthy condOut
        then interpret' thenStmt
        else return ()

interpret' (WhileStmt cond body) = do
    condOut <- evaluateM cond
    if isTruthy condOut
        then do
            interpret' body
            interpret' (WhileStmt cond body)
        else return ()

interpret :: [Stmt] -> IO (Either LoxError ())
interpret stmts = do
    (possibleErr, _) <- runStateT (runExceptT (mapM_ interpret' stmts)) emptyEnv
    return possibleErr

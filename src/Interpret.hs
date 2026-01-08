module Interpret
( interpret
) where

import Parse
import Token
import LoxError
import Environment
import Expr()
import Stmt()
import ExprOut
import Control.Monad.State
import Control.Monad.Except

-- Return statements are executed by throwing errors, same as in the book.
data InterpretError =
    Returned Token ExprOut
  | OtherErr LoxError
  deriving (Show)

-- Model statement as a state change from the initial environment
-- to a tuple of (possible error, new environment), with IO side-effects.
-- Environment -> IO (Either LoxError (), Environment)
-- There may be both an error and a new environment if e.g. the
-- environment was modified before the error occurred. This can
-- enable synchronization (i.e. continuing to interpret in spite
-- of errors) in future.
-- 
type Interpret = ExceptT InterpretError (StateT Environment IO)

throwRuntimeErr :: Token -> String -> Interpret a
throwRuntimeErr t msg = throwError (OtherErr (makeTokenErr t msg))

-- Expression evaluation helpers

handleBinary :: Token -> ExprOut -> ExprOut -> Interpret ExprOut
handleBinary op@(Token Plus _) e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ NumOut (n1 + n2)
    (NumOut _, _)          -> throwRuntimeErr op "Operand must be a number."

    (StrOut s1, StrOut s2) -> return $ StrOut (s1 ++ s2)
    (StrOut _, _)          -> throwRuntimeErr op "Operand must be a string."

    _                      -> throwRuntimeErr op "Operand must be a number or string."

handleBinary op@(Token Minus _) e1 e2 = binaryNumNumHelper (-) op e1 e2
handleBinary op@(Token Star _) e1 e2 = binaryNumNumHelper (*) op e1 e2

handleBinary op@(Token Slash _) e1 e2 = case e2 of
    NumOut 0 -> throwRuntimeErr op "Cannot divide by zero."
    _        -> binaryNumNumHelper (/) op e1 e2

handleBinary (Token EqualEqual _) e1 e2 = return $ BoolOut (e1 == e2)
handleBinary (Token BangEqual _) e1 e2 = return $ BoolOut (e1 /= e2)

handleBinary op@(Token Greater _) e1 e2 = binaryNumBoolHelper (>) op e1 e2
handleBinary op@(Token GreaterEqual _) e1 e2 = binaryNumBoolHelper (>=) op e1 e2
handleBinary op@(Token Less _) e1 e2 = binaryNumBoolHelper (<) op e1 e2
handleBinary op@(Token LessEqual _) e1 e2 = binaryNumBoolHelper (<=) op e1 e2

handleBinary t _ _ = throwRuntimeErr t "Unexpected token."

binaryNumNumHelper :: (Double -> Double -> Double) -> Token -> ExprOut -> ExprOut -> Interpret ExprOut
binaryNumNumHelper f op e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ NumOut (f n1 n2)
    _ -> throwRuntimeErr op "Operand must be a number."

binaryNumBoolHelper :: (Double -> Double -> Bool) -> Token -> ExprOut -> ExprOut -> Interpret ExprOut
binaryNumBoolHelper f op e1 e2 = case (e1, e2) of
    (NumOut n1, NumOut n2) -> return $ BoolOut (f n1 n2)
    _ -> throwRuntimeErr op "Operand must be a number."

-- As per spec, false and nil are falsey, everything else is truthy
isTruthy :: ExprOut -> Bool
isTruthy (BoolOut False) = False
isTruthy (NilOut)        = False
isTruthy _               = True

handleUnary :: Token -> ExprOut -> Interpret ExprOut
handleUnary op@(Token Minus _) e = case e of
    NumOut n -> return $ NumOut (-n)
    _        -> throwRuntimeErr op "Expected number for negation."

handleUnary (Token Bang _) e = return $ BoolOut ((not . isTruthy) e)

handleUnary token _ = throwRuntimeErr token "Unexpected token."

handleVar :: Token -> String ->Interpret ExprOut
handleVar t s = do
    env <- get
    case getVar env s of
        Nothing -> throwRuntimeErr t "Undefined variable."
        Just e  -> return e

handleAssign :: Expr -> ExprOut -> Interpret ExprOut
handleAssign lhs rhs = do
    env <- get
    case lhs of
        Variable t s -> do
            case assignVar env s rhs of
                Just env' -> put env'
                Nothing   -> throwRuntimeErr t "Undefined variable."
            return rhs
        _ -> throwError $ OtherErr (LoxError 1 "" "Invalid assignment target.")

handleLogical :: Token -> ExprOut -> ExprOut -> Interpret ExprOut
handleLogical (Token And _) e1 e2 =
    return $ BoolOut $ isTruthy e1 && isTruthy e2

handleLogical (Token Or _) e1 e2 =
    return $ BoolOut $ isTruthy e1 || isTruthy e2

handleLogical t _ _ = throwRuntimeErr t "Unexpected token."

catchFnError :: InterpretError -> Interpret ExprOut
catchFnError (Returned _ out) = return out
catchFnError err@(OtherErr _) = throwError err

callFunction :: Stmt -> Interpret ExprOut
callFunction stmt = do {
    interpret' stmt;
    return NilOut;
} `catchError` catchFnError

handleCall :: ExprOut -> Token -> [ExprOut] -> Interpret ExprOut
handleCall callee rightParen args = do
    case callee of
        FunOut _ params body -> do
            if length params /= length args
                then throwRuntimeErr rightParen (arityErr args params)
                else do
                    env <- pushCtx <$> get
                    let paramsAndArgs = zip params args
                        env' = foldr (\(p, a) acc -> define acc p a) env paramsAndArgs
                    put env'
                    returnVal <- callFunction body
                    env'' <- get
                    put (popCtx env'')
                    return returnVal
        _ -> throwRuntimeErr rightParen "Can only call functions and classes."
    where
        arityErr args' params =
            "Expected " ++ show (length params) ++ " arguments but got " ++ show (length args') ++ "."

evaluate :: Expr -> Interpret ExprOut
evaluate expr =
    case expr of
        LitNum n -> return $ NumOut n
        LitStr s -> return $ StrOut s
        LitBool b -> return $ BoolOut b
        LitNil -> return $ NilOut
        Grouping e -> evaluate e
        Unary op e -> do
            outR <- evaluate e
            handleUnary op outR
        Binary e1 op e2 -> do
            outL <- evaluate e1
            outR <- evaluate e2
            handleBinary op outL outR
        Variable t s -> handleVar t s
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
        Call callee rightParen args -> do
            calleeOut <- evaluate callee
            argsOut <- mapM evaluate args
            handleCall calleeOut rightParen argsOut

interpret' :: Stmt -> Interpret ()
interpret' (ExprStmt e) = do
    -- Throw away output of expression, e.g. 1 + 1;
    _ <- evaluate e
    return ()

interpret' (PrintStmt e) = do
    out <- evaluate e
    liftIO $ print out

interpret' (VarStmt s e) = do
    rhs <- evaluate e
    env <- get
    put (define env s rhs)

interpret' (VarStmtNoInit s) = do
    env <- get
    put (define env s NilOut)

interpret' (Block stmts) = do
    env <- get
    put (pushCtx env)
    -- Since we've pushed a context, we need to pop it even if there's an error.
    -- This ensures context pushes/pops work properly in nested blocks within
    -- functions with return statements.
    (mapM_ interpret' stmts) `catchError` \err -> do
        env' <- get
        put (popCtx env')
        throwError err
    env' <- get
    put (popCtx env')

interpret' (IfThenElse cond thenStmt elseStmt) = do
    condOut <- evaluate cond
    if isTruthy condOut
        then interpret' thenStmt
        else interpret' elseStmt

interpret' (IfThen cond thenStmt) = do
    condOut <- evaluate cond
    if isTruthy condOut
        then interpret' thenStmt
        else return ()

interpret' (WhileStmt cond body) = do
    condOut <- evaluate cond
    if isTruthy condOut
        then do
            interpret' body
            interpret' (WhileStmt cond body)
        else return ()

interpret' (FunStmt s params body) = do
    env <- get
    put (define env s (FunOut s params body))

interpret' (ReturnStmt returnT expr) = do
    out <- evaluate expr
    throwError (Returned returnT out)

interpret :: [Stmt] -> IO (Either LoxError ())
interpret stmts = do
    (possibleErr, _) <- runStateT (runExceptT (mapM_ interpret' stmts)) emptyEnv
    case possibleErr of
        Left (Returned returnT _) -> return $ Left (makeTokenErr returnT "Unexpected return statement.")
        Left (OtherErr err) -> return (Left err)
        Right _ -> return (Right ())

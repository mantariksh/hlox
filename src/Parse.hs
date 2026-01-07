module Parse
( parse
, Expr(..)
, Stmt(..)
) where

import Token
import LoxError
import Control.Monad.State
import Control.Monad.Except

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
  deriving (Eq)

data Stmt =
    ExprStmt Expr
  | PrintStmt Expr
  | VarStmt Token Expr
  | VarStmtNoInit Token
  | Block [Stmt]
  | IfThenElse Expr Stmt Stmt
  | IfThen Expr Stmt
  deriving (Eq, Show)

instance Show Expr where
    show (Literal (Token t _)) = show t
    show (Grouping e) = "(group " ++ show e ++ ")"
    show (Unary (Token op _) e) = '(':(show op) ++ show e ++ ")"
    show (Binary e1 (Token op _) e2) = '(':(show e1) ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Variable (Token s _)) = show s
    show (Assign lhs rhs) = show lhs ++ " = " ++ show rhs

-- Model parsing as a state change from a [Token] (representing the remaining tokens)
-- to another [Token], possibly producing an error.
-- [Token] -> Either LoxError a, [Token]
type Parse = ExceptT LoxError (State [Token])

peekT :: Parse Token
peekT = do
    tokens <- get
    case tokens of
        []   -> throwError (LoxError 1 "" "Expected EOF.")
        t:_ -> return t

popT :: Parse Token
popT = do
    tokens <- get
    case tokens of
        []   -> throwError (LoxError 1 "" "Expected EOF.")
        t:ts -> do
            put ts
            return t

throwTokenErr :: Token -> String -> Parse a
throwTokenErr t msg = throwError (makeTokenErr t msg)

primary :: Parse Expr
primary = do
    t <- popT
    case t of
        Token LeftParen _ -> do
            e <- expression
            t2 <- popT
            case t2 of
                Token RightParen _ ->
                    return (Grouping e)
                _ ->
                    throwTokenErr t "Expect ')' after expression."
        Token (Number _) _ ->
            return (Literal t)
        Token (String _) _ ->
            return (Literal t)
        Token TrueToken _ ->
            return (Literal t)
        Token FalseToken _ ->
            return (Literal t)
        Token Nil _ ->
            return (Literal t)
        Token (Identifier _) _ ->
            return (Variable t)
        _ ->
            throwTokenErr t "Expect expression."

unary :: Parse Expr
unary = do
    t <- peekT
    case t of
        Token Bang _ -> unary' t
        Token Minus _ -> unary' t
        _ -> primary
        where
            unary' op = do
                _ <- popT
                expr <- unary
                return (Unary op expr)

factor' :: Expr -> Parse Expr
factor' lhs = do
    t <- peekT
    case t of
        Token Star _ -> helper t
        Token Slash _ -> helper t
        _ -> return lhs
        where
            helper op = do
                _ <- popT
                rhs <- unary
                factor' (Binary lhs op rhs)

factor :: Parse Expr
factor = do
    lhs <- unary
    factor' lhs

term' :: Expr -> Parse Expr
term' lhs = do
    t <- peekT
    case t of
        Token Plus _ -> helper t
        Token Minus _ -> helper t
        _ -> return lhs
        where
            helper op = do
                _ <- popT
                rhs <- factor
                term' (Binary lhs op rhs)

term :: Parse Expr
term = do
    lhs <- factor
    term' lhs

comparison' :: Expr -> Parse Expr
comparison' lhs = do
    t <- peekT
    case t of
        Token Greater _ -> helper t
        Token GreaterEqual _ -> helper t
        Token Less _ -> helper t
        Token LessEqual _ -> helper t
        _ -> return lhs
        where
            helper op = do
                _ <- popT
                rhs <- term
                comparison' (Binary lhs op rhs)

comparison :: Parse Expr
comparison = do
    lhs <- term
    comparison' lhs

equality' :: Expr -> Parse Expr
equality' lhs = do
    t <- peekT
    case t of
        Token BangEqual _ -> helper t
        Token EqualEqual _ -> helper t
        _ -> return lhs
        where
            helper op = do
                _ <- popT
                rhs <- comparison
                equality' (Binary lhs op rhs)

equality :: Parse Expr
equality = do
    lhs <- comparison
    equality' lhs

assignment :: Parse Expr
assignment = do
    lhs <- equality
    t <- peekT
    case t of
        Token Equal _ -> do
            _ <- popT
            case lhs of
                Variable _ -> do
                    rhs <- assignment
                    return (Assign lhs rhs)
                _ -> throwTokenErr t "Invalid assignment target."
        _ -> return lhs

expression :: Parse Expr
expression = assignment

printStatement :: Parse Stmt
printStatement = do
    expr <- expression
    t <- popT
    case t of
        Token Semicolon _ ->
            return (PrintStmt expr)
        _ ->
            throwTokenErr t "Expect ';' after value."

expressionStatement :: Parse Stmt
expressionStatement = do
    expr <- expression
    t <- popT
    case t of
        Token Semicolon _ ->
            return (ExprStmt expr)
        _ ->
            throwTokenErr t "Expect ';' after expression."

block' :: [Stmt] -> Parse Stmt
block' stmts = do
    t <- peekT
    case t of
        Token RightBrace _ ->
            popT >> return (Block stmts)
        _ -> do
            stmt <- statement
            block' (stmt:stmts)

block :: Parse Stmt
block = block' []

ifStatement :: Parse Stmt
ifStatement = do
    t <- popT
    case t of
        Token LeftParen _ -> do
            cond <- expression
            t2 <- popT
            case t2 of
                Token RightParen _ -> do
                    thenStmt <- nonDeclaration
                    t3 <- peekT
                    case t3 of
                        Token Else _ -> do
                            _ <- popT
                            elseStmt <- nonDeclaration
                            return (IfThenElse cond thenStmt elseStmt)
                        _ ->
                            return (IfThen cond thenStmt)
                _ -> throwTokenErr t2 "Expect ')' after if condition."
        _ -> throwTokenErr t "Expect '(' after 'if'."

nonDeclaration :: Parse Stmt
nonDeclaration = do
    t <- peekT
    case t of
        Token If _ -> popT >> ifStatement
        Token Print _ -> popT >> printStatement
        Token LeftBrace _ -> popT >> block
        _ -> expressionStatement

declaration :: Parse Stmt
declaration = do
    t <- popT
    case t of
        Token (Identifier _) _ -> do
            t2 <- popT
            case t2 of
                Token Equal _ -> do
                    expr <- expression
                    t3 <- popT
                    case t3 of
                        Token Semicolon _ ->
                            return (VarStmt t expr)
                        _ -> throwTokenErr t3 "Expect ';' after variable declaration."
                Token Semicolon _ ->
                    return (VarStmtNoInit t)
                _ -> throwTokenErr t2 "Expect '=' or ';' after variable name."
        _ -> throwTokenErr t "Expect variable name."

-- The book uses "statement" vs "declaration" somewhat confusingly.
-- Here we use "statement" as the top-level construct, which then
-- splits into declarations and non-declarations.
statement :: Parse Stmt
statement = do
    t <- peekT
    case t of
        Token Var _ -> popT >> declaration
        _           -> nonDeclaration

parse :: [Token] -> Either LoxError [Stmt]
parse [Token EOF _] = return []
parse ts = do
    let (result, ts') = runState (runExceptT statement) ts
    case result of
        Left err -> Left err
        Right stmt -> do
            stmts <- parse ts'
            return (stmt:stmts)

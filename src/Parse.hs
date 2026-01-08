module Parse
( parse
, Expr(..)
, Stmt(..)
) where

import Token
import LoxError
import Control.Monad.State
import Control.Monad.Except
import Expr
import Stmt

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

popOrThrow :: TokenType -> String -> Parse ()
popOrThrow tType msg = do
    t@(Token tType' _) <- popT
    if tType == tType'
        then return ()
        else throwTokenErr t msg

primary :: Parse Expr
primary = do
    t <- popT
    case t of
        Token LeftParen _ -> do
            e <- expression
            popOrThrow RightParen "Expect ')' after expression."
            return (Grouping e)
        Token (Number n) _ ->
            return (LitNum n)
        Token (String s) _ ->
            return (LitStr s)
        Token TrueToken _ ->
            return (LitBool True)
        Token FalseToken _ ->
            return (LitBool False)
        Token Nil _ ->
            return LitNil
        Token (Identifier s) _ ->
            return (Variable t s)
        _ ->
            throwTokenErr t "Expect expression."

argList :: Parse ([Expr], Token)
argList = do
    t@(Token tType _) <- peekT
    -- This outer if statement handles the case of 0 args
    if tType == RightParen
        then do
            _ <- popT
            return ([], t)
        -- Helper handles >0 args
        else argList' []
    where
        argList' args = do
            arg <- expression
            nextT <- popT
            case nextT of
                Token Comma _ ->
                    argList' (arg:args)
                Token RightParen _ ->
                    return (reverse (arg:args), nextT)
                _ ->
                    throwTokenErr nextT "Expected ')' after arguments."                    

call' :: Expr -> Parse Expr
call' lhs = do
    Token tType _ <- peekT
    if tType == LeftParen
        then do
            _ <- popT
            (args, rightParen) <- argList
            call' (Call lhs rightParen args)
        else return lhs

call :: Parse Expr
call = do
    expr <- primary
    call' expr

unary :: Parse Expr
unary = do
    t <- peekT
    case t of
        Token Bang _ -> unary' t
        Token Minus _ -> unary' t
        _ -> call
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

andExpr' :: Expr -> Parse Expr
andExpr' lhs = do
    t@(Token tType _) <- peekT
    if tType == And
        then do
            _ <- popT
            rhs <- equality
            andExpr' (Logical lhs t rhs)
        else return lhs

andExpr :: Parse Expr
andExpr = do
    lhs <- equality
    andExpr' lhs

orExpr' :: Expr -> Parse Expr
orExpr' lhs = do
    t@(Token tType _) <- peekT
    if tType == Or
        then do
            _ <- popT
            rhs <- andExpr
            orExpr' (Logical lhs t rhs)
        else return lhs

orExpr :: Parse Expr
orExpr = do
    lhs <- andExpr
    orExpr' lhs

assignment :: Parse Expr
assignment = do
    lhs <- orExpr
    t <- peekT
    case t of
        Token Equal _ -> do
            _ <- popT
            case lhs of
                Variable _ _ -> do
                    rhs <- assignment
                    return (Assign lhs rhs)
                _ -> throwTokenErr t "Invalid assignment target."
        _ -> return lhs

expression :: Parse Expr
expression = assignment

printStatement :: Parse Stmt
printStatement = do
    expr <- expression
    popOrThrow Semicolon "Expect ';' after value."
    return (PrintStmt expr)

expressionStatement :: Parse Stmt
expressionStatement = do
    expr <- expression
    popOrThrow Semicolon "Expect ';' after expression."
    return (ExprStmt expr)

-- Accumulates block statements in reverse order
block' :: [Stmt] -> Parse Stmt
block' stmts = do
    t <- peekT
    case t of
        Token RightBrace _ ->
            popT >> return (Block (reverse stmts))
        _ -> do
            stmt <- statement
            block' (stmt:stmts)

block :: Parse Stmt
block = block' []

ifStatement :: Parse Stmt
ifStatement = do
    popOrThrow LeftParen "Expect '(' after 'if'."
    cond <- expression
    popOrThrow RightParen "Expect ')' after if condition."
    thenStmt <- nonDeclaration
    Token tType _ <- peekT
    if tType == Else
        then do
            _ <- popT
            elseStmt <- nonDeclaration
            return (IfThenElse cond thenStmt elseStmt)
        else return (IfThen cond thenStmt)

whileStatement :: Parse Stmt
whileStatement = do
    popOrThrow LeftParen "Expect '(' after 'while'."
    cond <- expression
    popOrThrow RightParen "Expect ')' after while condition."
    body <- nonDeclaration
    return (WhileStmt cond body)

-- Desugars for loops to while statements
forStatement :: Parse Stmt
forStatement = do
    popOrThrow LeftParen "Expect '(' after 'for'."
    initStmt <- getInit
    condExpr <- getCond
    incStmt <- getInc
    body <- nonDeclaration
    -- We need the init statement to run before the while statement,
    -- and the increment statement to run after each execution of the body.
    let whileStmt = WhileStmt condExpr (Block [body, incStmt])
    return $ Block [initStmt, whileStmt]
    where
        getInit = do
            Token tType _ <- peekT
            initStmt tType
            where
                initStmt tType
                    | tType == Semicolon =
                        -- No initializer, so return a statement that does nothing
                        popT >> return (ExprStmt LitNil)
                    | tType == Var =
                        popT >> varDeclaration
                    | otherwise =
                        expressionStatement
        getCond = do
            Token tType _ <- peekT
            if tType == Semicolon
                -- No condition, so always true
                then do
                    _ <- popT
                    return (LitBool True)
                else do
                    cond <- expression
                    popOrThrow Semicolon "Expect ';' after loop condition."
                    return cond
        getInc = do
            Token tType _ <- peekT
            if tType == RightParen
                -- No increment, so return a statement that does nothing
                then do
                    _ <- popT
                    return (ExprStmt LitNil)
                else do
                    inc <- expression
                    popOrThrow RightParen "Expect ')' after for clauses."
                    return (ExprStmt inc)

returnStatement :: Token -> Parse Stmt
returnStatement returnT = do
    Token tType _ <- peekT
    if tType == Semicolon
        then do
            _ <- popT
            return (ReturnStmt returnT LitNil)
        else do
            expr <- expression
            popOrThrow Semicolon "Expect ';' after return value."
            return (ReturnStmt returnT expr)

nonDeclaration :: Parse Stmt
nonDeclaration = do
    t <- peekT
    case t of
        Token For _ -> popT >> forStatement
        Token If _ -> popT >> ifStatement
        Token Print _ -> popT >> printStatement
        Token Return _ -> popT >>= returnStatement
        Token While _ -> popT >> whileStatement
        Token LeftBrace _ -> popT >> block
        _ -> expressionStatement

varDeclaration :: Parse Stmt
varDeclaration = do
    t <- popT
    case t of
        Token (Identifier s) _ -> do
            t2 <- popT
            case t2 of
                Token Equal _ -> do
                    expr <- expression
                    popOrThrow Semicolon "Expect ';' after variable declaration."
                    return (VarStmt s expr)
                Token Semicolon _ ->
                    return (VarStmtNoInit s)
                _ -> throwTokenErr t2 "Expect '=' or ';' after variable name."
        _ -> throwTokenErr t "Expect variable name."

paramList' :: [String] -> Parse [String]
paramList' params = do
    param <- popT
    case param of
        Token (Identifier s) _ -> do
            nextT <- popT
            case nextT of
                Token Comma _ ->
                    paramList' (s:params)
                Token RightParen _ ->
                    return (reverse (s:params))
                _ ->
                    throwTokenErr nextT "Expect ')' after parameters."
        _ -> throwTokenErr param "Expect parameter name."

paramList :: Parse [String]
paramList = do
    t <- peekT
    case t of
        -- Handle 0 param case
        Token RightParen _ -> popT >> return []
        -- Helper handles >0 params
        _ -> paramList' []

funDeclaration :: String -> Parse Stmt
funDeclaration kind = do
    t <- popT
    case t of
        Token (Identifier s) _ -> do
            popOrThrow LeftParen ("Expect '(' after " ++ kind ++ " name.")
            params <- paramList
            popOrThrow LeftBrace ("Expect '{' before " ++ kind ++ " body.")
            body <- block
            return (FunStmt s params body)
        _ -> throwTokenErr t ("Expect " ++ kind ++ " name.")

-- The book uses "statement" vs "declaration" somewhat confusingly.
-- Here we use "statement" as the top-level construct, which then
-- splits into declarations and non-declarations.
statement :: Parse Stmt
statement = do
    t <- peekT
    case t of
        Token Var _ -> popT >> varDeclaration
        Token Fun _ -> popT >> funDeclaration "function"
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

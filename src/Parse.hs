module Parse
( parse
, Expr(..)
, Stmt(..)
) where

import Token
import LoxError

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
  deriving (Eq, Show)

instance Show Expr where
    show (Literal (Token t _)) = show t
    show (Grouping e) = "(group " ++ show e ++ ")"
    show (Unary (Token op _) e) = '(':(show op) ++ show e ++ ")"
    show (Binary e1 (Token op _) e2) = '(':(show e1) ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Variable (Token s _)) = show s
    show (Assign lhs rhs) = show lhs ++ " = " ++ show rhs

type RecursiveDescent = [Token] -> Either LoxError (Expr, [Token])
type RecursiveDescentMatcher = Expr -> [Token] -> Either LoxError (Expr, [Token])

type StmtState = Either LoxError (Stmt, [Token])

primary :: RecursiveDescent
primary ts = case ts of
    ((Token LeftParen ln):ts1) -> do
        (e, ts2) <- expression ts1
        case ts2 of
            ((Token RightParen _):ts3) ->
                return (Grouping e, ts3)
            (t:_) ->
                Left (makeTokenErr t "Expect ')' after expression.")
            _ ->
                Left (LoxError ln "" "Expect ')' after expression.")
    (t@(Token (Number _) _):ts1) -> 
        return (Literal t, ts1)
    (t@(Token (String _) _):ts1) ->
        return (Literal t, ts1)
    (t@(Token TrueToken _):ts1) ->
        return (Literal t, ts1)
    (t@(Token FalseToken _):ts1) ->
        return (Literal t, ts1)
    (t@(Token Nil _):ts1) ->
        return (Literal t, ts1)
    (t@(Token (Identifier _) _):ts1) ->
        return (Variable t, ts1)
    (t:_) -> 
        Left (makeTokenErr t "Expect expression.")
    [] ->
        Left (LoxError 1 "" "Expect expression.")

unary :: RecursiveDescent
unary ts = case ts of
            (t@(Token Bang _):ts1) -> helper ts1 t
            (t@(Token Minus _):ts1) -> helper ts1 t
            _ -> primary ts
            where
                helper ts1 op = do
                    (expr, ts2) <- unary ts1
                    return (Unary op expr, ts2)

factor' :: RecursiveDescentMatcher
factor' lhs ts1 = case ts1 of
    (t@(Token Star _):ts2) -> helper ts2 t
    (t@(Token Slash _):ts2) -> helper ts2 t
    _ -> return (lhs, ts1)
    where
        helper ts2 op = do
            (rhs, ts3) <- unary ts2
            factor' (Binary lhs op rhs) ts3

factor :: RecursiveDescent
factor ts = do
    (lhs, ts1) <- unary ts
    factor' lhs ts1

term' :: RecursiveDescentMatcher
term' lhs ts1 = case ts1 of
    (t@(Token Plus _):ts2) -> helper ts2 t
    (t@(Token Minus _):ts2) -> helper ts2 t
    _ -> return (lhs, ts1)
    where
        helper ts2 op = do
            (rhs, ts3) <- factor ts2
            term' (Binary lhs op rhs) ts3

term :: RecursiveDescent
term ts = do
    (lhs, ts1) <- factor ts
    term' lhs ts1

comparison' :: RecursiveDescentMatcher
comparison' lhs ts1 = case ts1 of
    (t@(Token Greater _):ts2) -> helper ts2 t
    (t@(Token GreaterEqual _):ts2) -> helper ts2 t
    (t@(Token Less _):ts2) -> helper ts2 t
    (t@(Token LessEqual _):ts2) -> helper ts2 t
    _ -> return (lhs, ts1)
    where
        helper ts2 op = do
            (rhs, ts3) <- term ts2
            comparison' (Binary lhs op rhs) ts3

comparison :: RecursiveDescent
comparison ts = do
    (lhs, ts1) <- term ts
    comparison' lhs ts1

equality' :: RecursiveDescentMatcher
equality' lhs ts1 = case ts1 of
    (t@(Token BangEqual _):ts2) -> helper ts2 t
    (t@(Token EqualEqual _):ts2) -> helper ts2 t
    _ -> return (lhs, ts1)
    where
        helper ts2 op = do
            (rhs, ts3) <- comparison ts2
            equality' (Binary lhs op rhs) ts3

equality :: RecursiveDescent
equality ts = do
    (lhs, ts1) <- comparison ts
    equality' lhs ts1

assignment :: RecursiveDescent
assignment ts = do
    (lhs, ts1) <- equality ts
    case ts1 of
        (t@(Token Equal _):ts2) -> case lhs of
            v@(Variable _) -> do
                (rhs, ts3) <- assignment ts2
                return (Assign v rhs, ts3)
            _ ->
                Left (makeTokenErr t "Invalid assignment target.")
        _ ->
            return (lhs, ts1)

expression :: RecursiveDescent
expression = assignment

printStatement :: Token -> [Token] -> StmtState
printStatement t ts1 = do
    (expr, ts2) <- expression ts1
    case ts2 of
        ((Token Semicolon _):ts3) ->
            return (PrintStmt expr, ts3)
        _ ->
            Left (makeTokenErr t "Expect ';' after value.")

expressionStatement :: [Token] -> StmtState
expressionStatement ts = do
    (expr, ts1) <- expression ts
    case ts1 of
        ((Token Semicolon _):ts2) ->
            return (ExprStmt expr, ts2)
        (t:_) ->
            Left (makeTokenErr t "Expect ';' after expression.")
        [] ->
            Left (LoxError 1 "" "Expect ';' after expression.")

nonDeclaration :: [Token] -> StmtState
nonDeclaration ts = case ts of
    (t@(Token Print _):ts1) -> 
        printStatement t ts1
    _ -> expressionStatement ts

declaration :: [Token] -> StmtState
declaration ts = case ts of
    (idToken@(Token (Identifier _) _):ts1) -> case ts1 of
        (Token Equal _):ts2 -> do
            (initExpr, ts3) <- expression ts2
            case ts3 of
                (Token Semicolon _):ts4 ->
                    return (VarStmt idToken initExpr, ts4)
                (t:_) ->
                    Left (makeTokenErr t "Expect ';' after variable declaration.")
                [] ->
                    Left (LoxError 1 "" "Expect ';' after variable declaration.")
        (Token Semicolon _):ts2 ->
            return (VarStmtNoInit idToken, ts2)
        (t:_) ->
            Left (makeTokenErr t "Expect '=' or ';' after variable name.")
        [] ->
            Left (LoxError 1 "" "Expect '=' or ';' after variable name.")
    (t:_) ->
        Left (makeTokenErr t "Expect variable name.")
    [] ->
        Left (LoxError 1 "" "Expect variable name.")


statement :: [Token] -> StmtState
statement ts = case ts of
    ((Token Var _):ts1) -> 
        declaration ts1
    _ ->
        nonDeclaration ts

-- The book uses "statement" vs "declaration" somewhat confusingly.
-- Here we use "statement" as the top-level construct, which then
-- splits into declarations and non-declarations.


parse :: [Token] -> Either LoxError [Stmt]
parse [Token EOF _] = return []
parse ts = do
    (stmt, ts1) <- statement ts
    stmts <- parse ts1
    return (stmt:stmts)

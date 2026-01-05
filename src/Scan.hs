-- Check out archive/Scanner.hs for the old version of this, which
-- I tried to implement without properly understanding monads and
-- recursion in Haskell. Big difference!
-- I'm sure there are many more things I can improve to make this
-- more idiomatic but I'm happy with it for now.

module Scan
( scan
)
where

import LoxError
import Token
import Control.Monad
import Data.Char

type LineNo = Int
type RevString = [Char]
type RevTokens = [Token]
data CurrScanning = 
    Empty
  -- We've seen a / but not yet a //
  | MaybeComment
  | InComment
  | SingleCharToken Char
  -- e.g. we've seen a ! which may end up being a !=
  | MaybeTwoCharToken Char
  | TwoCharToken Char Char
  | InString RevString
  -- Decimal point not added yet
  | InNumberPreDp RevString
  -- Decimal point added but no digits after it
  | InNumberNoDp RevString
  -- Decimal point added and digits after it
  | InNumberPostDp RevString
  | InKeywordOrIdentifier RevString

type ScanState = (CurrScanning, LineNo, RevTokens)

-- When we see a new character, what state should we start from for the next token?
restartChar :: Char -> LineNo -> Either LoxError (CurrScanning, LineNo)
restartChar c ln
    | c == '\n' = return (Empty, ln + 1)
    | c == '/' = return (MaybeComment, ln)
    | c == '"' = return (InString "", ln)
    | isSpace c = return (Empty, ln)
    | isMaybeTwoCharToken c = return (MaybeTwoCharToken c, ln)
    | isSingleCharToken c = return (SingleCharToken c, ln)
    | isDigit c = return (InNumberPreDp [c], ln)
    | isAlpha c || c == '_' = return (InKeywordOrIdentifier [c], ln)
    | otherwise = Left (LoxError ln "" ("Unexpected token: " ++ [c]))

isSingleCharToken :: Char -> Bool
isSingleCharToken c = c `elem` ['(', ')', '{', '}', ',', '.', '-', '+', ';', '*']

isMaybeTwoCharToken :: Char -> Bool
isMaybeTwoCharToken c = c `elem` ['!', '=', '<', '>']

isTwoCharToken :: CurrScanning -> Bool
isTwoCharToken (TwoCharToken c1 c2) = c1:[c2] `elem` ["!=", "==", ">=", "<="]
isTwoCharToken _ = False

toNumber :: LineNo -> RevString -> Token
toNumber ln s = Token (Number (read (reverse s))) ln

toString :: LineNo -> RevString -> Token
toString ln s = Token (String (reverse s)) ln

makeUnterminatedStringError :: LineNo -> RevString -> LoxError
makeUnterminatedStringError ln s = LoxError ln "" ("Unterminated string: " ++ reverse s)

-- Assumes that CurrScanning contains a valid token
-- Flushes the currently accumulated token into the list of accumulated tokens
updateTokens :: LineNo -> CurrScanning -> RevTokens -> RevTokens
updateTokens _ Empty out = out

-- Comments
updateTokens ln MaybeComment out = (Token Slash ln):out
updateTokens _ InComment out = out

-- Single-character tokens
updateTokens ln (SingleCharToken c) out =
    let tokenise t = (Token t ln):out
    in case c of
        '(' -> tokenise LeftParen
        ')' -> tokenise RightParen
        '{' -> tokenise LeftBrace
        '}' -> tokenise RightBrace
        ',' -> tokenise Comma
        '.' -> tokenise Dot
        '-' -> tokenise Minus
        '+' -> tokenise Plus
        ';' -> tokenise Semicolon
        '*' -> tokenise Star
        -- We shouldn't reach this case
        _ -> out

-- Maybe two-character tokens which ended up being only one character
updateTokens ln (MaybeTwoCharToken c) out =
    let tokenise t = (Token t ln):out
    in case c of
        '!' -> tokenise Bang
        '=' -> tokenise Equal
        '>' -> tokenise Greater
        '<' -> tokenise Less
        -- We shouldn't reach this case
        _ -> out
-- Two-character tokens
updateTokens ln (TwoCharToken c1 c2) out =
    let tokenise t = (Token t ln):out
    in case c1:[c2] of
        "!=" -> tokenise BangEqual
        "==" -> tokenise EqualEqual
        ">=" -> tokenise GreaterEqual
        "<=" -> tokenise LessEqual
        -- We shouldn't reach this case
        _ -> out

updateTokens ln (InString s) out = (toString ln s):out
-- We may need to handle numbers in updateTokens if the program ends with
-- a number.
updateTokens ln (InNumberPreDp s) out = (toNumber ln s):out
-- We saw a number like "123.", but no digits after the decimal point.
-- This means we need to treat the "123" as a valid number token followed
-- by a dot.
updateTokens ln (InNumberNoDp s) out = (Token Dot ln):(toNumber ln s):out
updateTokens ln (InNumberPostDp s) out = (toNumber ln s):out
updateTokens ln (InKeywordOrIdentifier s) out =
    let tokenise t = (Token t ln):out
        s' = reverse s
    in case s' of
        "and" -> tokenise And
        "class" -> tokenise Class
        "else" -> tokenise Else
        "false" -> tokenise FalseToken
        "fun" -> tokenise Fun
        "for" -> tokenise For
        "if" -> tokenise If
        "nil" -> tokenise Nil
        "or" -> tokenise Or
        "print" -> tokenise Print
        "return" -> tokenise Return
        "super" -> tokenise Super
        "this" -> tokenise This
        "true" -> tokenise TrueToken
        "var" -> tokenise Var
        "while" -> tokenise While
        _   -> tokenise (Identifier s')

-- Given the current state and the next character, generates the next state
next :: ScanState -> Char -> Either LoxError ScanState
next (Empty, ln, out) c = do
    (nextState, ln') <- restartChar c ln
    return (nextState, ln', out)

-- Parsing a comment
next (MaybeComment, ln, out) c
    | c == '/' = return (InComment, ln, out)
    | otherwise = do
        (nextState, ln') <- restartChar c ln
        return (nextState, ln', updateTokens ln MaybeComment out)
next (InComment, ln, out) c
    | c == '\n' = return (Empty, ln + 1, out)
    | otherwise = return (InComment, ln, out)

next (SingleCharToken c, ln, out) c2 = do
    (nextState, ln') <- restartChar c2 ln
    return (nextState, ln', updateTokens ln (SingleCharToken c) out)

-- Determine whether MaybeTwoChar is one or two chars
next (MaybeTwoCharToken c1, ln, out) c2
    | isTwoCharToken charAdded = return (Empty, ln, updateTokens ln charAdded out)
    | otherwise = do
        (nextState, ln') <- restartChar c2 ln
        return (nextState, ln', updateTokens ln (MaybeTwoCharToken c1) out)
    where charAdded = TwoCharToken c1 c2

next (TwoCharToken c1 c2, ln, out) c3 = do
    (nextState, ln') <- restartChar c3 ln
    return (nextState, ln', updateTokens ln (TwoCharToken c1 c2) out)

-- In the middle of a string literal
next (InString s, ln, out) c
    | c == '\"' = return (Empty, ln, updateTokens ln (InString s) out)
    | c == '\n' = return (InString (c:s), ln + 1, out)
    | otherwise = return (InString (c:s), ln, out)

-- In the middle of a number literal
next (InNumberPreDp s, ln, out) c
    | c == '\n' = return (Empty, ln + 1, flushed)
    | isDigit c = return (InNumberPreDp (c:s), ln, out)
    | c == '.' = return (InNumberNoDp s, ln, out)
    | otherwise = do
        (nextState, ln') <- restartChar c ln
        return (nextState, ln', flushed)
    where flushed = updateTokens ln (InNumberPreDp s) out

next (InNumberNoDp s, ln, out) c
    | c == '\n' = return (Empty, ln + 1, flushed)
    | isDigit c = return (InNumberPostDp (c:'.':s), ln, out)
    | otherwise = do
        (nextState, ln') <- restartChar c ln
        return (nextState, ln', flushed)
    where flushed = updateTokens ln (InNumberNoDp s) out

next (InNumberPostDp s, ln, out) c
    | c == '\n' = return (Empty, ln + 1, flushed)
    | isDigit c = return (InNumberPostDp (c:s), ln, out)
    | otherwise = do
        (nextState, ln') <- restartChar c ln
        return (nextState, ln', flushed)
    where flushed = updateTokens ln (InNumberPostDp s) out

-- Any other token
next (InKeywordOrIdentifier s, ln, out) c
    | c == '\n' =
        return (Empty, ln + 1, flushed)
    | isDigit c || isAlpha c || c == '_' =
        return (InKeywordOrIdentifier (c:s), ln, out)
    | otherwise = do
        (nextState, ln') <- restartChar c ln
        return (nextState, ln', flushed)
    where flushed = updateTokens ln (InKeywordOrIdentifier s) out

scan :: String -> Either LoxError [Token]
scan s = case foldM next (Empty, 1, []) s of
            Left err -> Left err
            -- Cases where the final unprocessed token is invalid
            Right ((InString s'), ln, _) -> Left (makeUnterminatedStringError ln s')
            -- Success case
            Right (unprocessed, ln, tokens) ->
                return (reverse ((Token EOF ln):updateTokens ln unprocessed tokens))

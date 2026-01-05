import Test.QuickCheck
import qualified Token as T
import Scan

-- Helper to check that scanning produces exactly one token (plus EOF)
scansToSingleToken :: String -> T.TokenType -> Bool
scansToSingleToken input expectedType =
    case scan input of
        Left _ -> False
        Right tokens -> 
            case tokens of
                [T.Token t1 _, T.Token T.EOF _] -> t1 == expectedType
                _ -> False

-- Generators for each token type

-- Single-character tokens
genSingleCharToken :: Gen (String, T.TokenType)
genSingleCharToken = elements [
    ("(", T.LeftParen),
    (")", T.RightParen),
    ("{", T.LeftBrace),
    ("}", T.RightBrace),
    (",", T.Comma),
    (".", T.Dot),
    ("-", T.Minus),
    ("+", T.Plus),
    (";", T.Semicolon),
    ("/", T.Slash),
    ("*", T.Star)
    ]

-- Two-character tokens (single char)
genTwoCharTokenSingle :: Gen (String, T.TokenType)
genTwoCharTokenSingle = elements [
    ("!", T.Bang),
    ("=", T.Equal),
    (">", T.Greater),
    ("<", T.Less)
    ]

-- Two-character tokens (two chars)
genTwoCharTokenDouble :: Gen (String, T.TokenType)
genTwoCharTokenDouble = elements [
    ("!=", T.BangEqual),
    ("==", T.EqualEqual),
    (">=", T.GreaterEqual),
    ("<=", T.LessEqual)
    ]

-- Keywords
genKeyword :: Gen (String, T.TokenType)
genKeyword = elements [
    ("and", T.And),
    ("class", T.Class),
    ("else", T.Else),
    ("false", T.FalseToken),
    ("fun", T.Fun),
    ("for", T.For),
    ("if", T.If),
    ("nil", T.Nil),
    ("or", T.Or),
    ("print", T.Print),
    ("return", T.Return),
    ("super", T.Super),
    ("this", T.This),
    ("true", T.TrueToken),
    ("var", T.Var),
    ("while", T.While)
    ]

-- Identifiers (must not be keywords, start with letter or underscore, contain letters/digits/underscore)
genIdentifier :: Gen (String, T.TokenType)
genIdentifier = sized $ \n -> do
    first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
    rest <- resize (min n 20) $ listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
    let ident = first : rest
    -- Make sure it's not a keyword
    if ident `elem` ["and", "class", "else", "false", "fun", "for", "if", "nil", "or", "print", "return", "super", "this", "true", "var", "while"]
        then genIdentifier  -- Retry if it's a keyword
        else return (ident, T.Identifier ident)

-- String literals
genStringLiteral :: Gen (String, T.TokenType)
genStringLiteral = do
    content <- listOf $ suchThat arbitrary (\c -> c /= '"' && c /= '\n')
    return ("\"" ++ content ++ "\"", T.String content)

-- Number literals (integers)
genIntegerLiteral :: Gen (String, T.TokenType)
genIntegerLiteral = do
    digits <- listOf1 $ elements ['0'..'9']
    let num = read digits :: Double
    return (digits, T.Number num)

-- Number literals (decimals)
genDecimalLiteral :: Gen (String, T.TokenType)
genDecimalLiteral = do
    before <- listOf1 $ elements ['0'..'9']
    after <- listOf1 $ elements ['0'..'9']
    let num = read (before ++ "." ++ after) :: Double
    return (before ++ "." ++ after, T.Number num)

-- Properties for each token type

prop_singleCharToken :: Property
prop_singleCharToken = forAll genSingleCharToken $ \(input, expectedType) ->
    scansToSingleToken input expectedType

prop_twoCharTokenSingle :: Property
prop_twoCharTokenSingle = forAll genTwoCharTokenSingle $ \(input, expectedType) ->
    scansToSingleToken input expectedType

prop_twoCharTokenDouble :: Property
prop_twoCharTokenDouble = forAll genTwoCharTokenDouble $ \(input, expectedType) ->
    scansToSingleToken input expectedType

prop_keyword :: Property
prop_keyword = forAll genKeyword $ \(input, expectedType) ->
    scansToSingleToken input expectedType

prop_identifier :: Property
prop_identifier = forAll genIdentifier $ \(input, expectedType) ->
    scansToSingleToken input expectedType

prop_stringLiteral :: Property
prop_stringLiteral = forAll genStringLiteral $ \(input, expectedType) ->
    scansToSingleToken input expectedType

prop_integerLiteral :: Property
prop_integerLiteral = forAll genIntegerLiteral $ \(input, expectedType) ->
    scansToSingleToken input expectedType

prop_decimalLiteral :: Property
prop_decimalLiteral = forAll genDecimalLiteral $ \(input, expectedType) ->
    scansToSingleToken input expectedType

main :: IO ()
main = do
    putStrLn "Testing single-character tokens..."
    quickCheck prop_singleCharToken
    
    putStrLn "Testing two-character tokens (single char)..."
    quickCheck prop_twoCharTokenSingle
    
    putStrLn "Testing two-character tokens (double char)..."
    quickCheck prop_twoCharTokenDouble
    
    putStrLn "Testing keywords..."
    quickCheck prop_keyword
    
    putStrLn "Testing identifiers..."
    quickCheck prop_identifier
    
    putStrLn "Testing string literals..."
    quickCheck prop_stringLiteral
    
    putStrLn "Testing integer literals..."
    quickCheck prop_integerLiteral
    
    putStrLn "Testing decimal literals..."
    quickCheck prop_decimalLiteral

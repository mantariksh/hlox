module LoxError
( LoxError (..)
, reportErr
, makeTokenErr
) where

import Token

type LineNo = Int

data LoxError = NoError | LoxError LineNo String String deriving (Show)

makeTokenErr :: Token -> String -> LoxError
makeTokenErr (Token EOF ln) msg =
    LoxError ln " at end" msg
makeTokenErr (Token t     ln) msg =
    LoxError ln (" at '" ++ show t ++ "'") msg

reportErr :: LoxError -> IO ()
reportErr NoError = return ()
reportErr (LoxError ln loc msg)
    = putStrLn
        ("[line " ++ show ln ++ "] Error"
            ++ loc ++ ": " ++ msg)

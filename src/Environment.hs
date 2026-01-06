module Environment
( Environment
, define
, getVar
, emptyEnv
) where

import qualified Data.Map as M
import Parse
import LoxError
import Token

type Environment = [M.Map String Expr]

emptyEnv :: Environment
emptyEnv = [M.empty]

define :: Environment -> String -> Expr -> Environment
define [] s e = [M.insert s e M.empty]
define (env:envs) s e = (M.insert s e env):envs

getVar :: Environment -> Token -> Either LoxError Expr
getVar [] t = Left (makeTokenErr t "Undefined variable.")
getVar (env:envs) t@(Token (Identifier s) _) =
    case M.lookup s env of
        Nothing -> getVar envs t
        Just e  -> return e
getVar _ t = Left (makeTokenErr t "Expected identifier.")

module Environment
( Environment
, define
, getVar
, emptyEnv
, assignVar
, pushCtx
, popCtx
) where

import ExprOut
import qualified Data.Map as M
import LoxError
import Token

type Environment = [M.Map String ExprOut]

emptyEnv :: Environment
emptyEnv = [M.empty]

define :: Environment -> String -> ExprOut -> Environment
define [] s e = [M.insert s e M.empty]
define (env:envs) s e = M.insert s e env:envs

getVar :: Environment -> Token -> Either LoxError ExprOut
getVar [] t = Left (makeTokenErr t "Undefined variable.")
getVar (env:envs) t@(Token (Identifier s) _) =
    case M.lookup s env of
        Nothing -> getVar envs t
        Just e  -> return e
getVar _ t = Left (makeTokenErr t "Expected identifier.")

assignVar :: Environment -> Token -> ExprOut -> Either LoxError Environment
assignVar [] t _ = Left (makeTokenErr t "Undefined variable.")
assignVar (env:envs) t@(Token (Identifier s) _) e =
    case M.lookup s env of
        Nothing -> do
            envs' <- assignVar envs t e
            return (env:envs')
        Just _ -> return (M.insert s e env:envs)
assignVar _ t _ = Left (makeTokenErr t "Expected identifier.")

pushCtx :: Environment -> Environment
pushCtx envs = M.empty:envs

popCtx :: Environment -> Environment
popCtx [] = []
popCtx (_:envs) = envs

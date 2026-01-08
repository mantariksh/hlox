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

type Environment = [M.Map String ExprOut]

emptyEnv :: Environment
emptyEnv = [M.empty]

define :: Environment -> String -> ExprOut -> Environment
define [] s e = [M.insert s e M.empty]
define (env:envs) s e = M.insert s e env:envs

getVar :: Environment -> String -> Maybe ExprOut
getVar [] _ = Nothing
getVar (env:envs) s =
    case M.lookup s env of
        Nothing -> getVar envs s
        Just e  -> Just e

assignVar :: Environment -> String -> ExprOut -> Maybe Environment
assignVar [] _ _ = Nothing
assignVar (env:envs) s e =
    case M.lookup s env of
        Nothing -> do
            envs' <- assignVar envs s e
            return (env:envs')
        Just _ -> return (M.insert s e env:envs)

pushCtx :: Environment -> Environment
pushCtx envs = M.empty:envs

popCtx :: Environment -> Environment
popCtx [] = []
popCtx (_:envs) = envs

module Eval where

import Abstract
import Value
import {-# SOURCE #-} TCM (TypeCheck)


reval :: Val -> TypeCheck Val
reEnv :: Env -> TypeCheck Env

toExpr :: Val -> TypeCheck Expr

whnf  :: Env -> Expr -> TypeCheck Val
whnf' :: Expr -> TypeCheck Val
app   :: Val -> Val -> TypeCheck Val
whnfClos :: Val -> TypeCheck Val
force :: Val -> TypeCheck Val
piApps :: TVal -> [Clos] -> TypeCheck TVal

projectType :: TVal -> Name -> TypeCheck TVal

up    :: Bool -> Val -> TVal -> TypeCheck Val

leqSize' :: Val -> Val -> TypeCheck ()

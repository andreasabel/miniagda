{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Eval where

import Abstract
import Value
import {-# SOURCE #-} TCM (TypeCheck)

class Reval a where
  reval' :: Valuation -> a -> TypeCheck a
  reval  :: a -> TypeCheck a
  reval = reval' emptyVal

instance Reval Val
instance Reval Env

toExpr :: Val -> TypeCheck Expr

whnf  :: Env -> Expr -> TypeCheck Val
whnf' :: Expr -> TypeCheck Val
app   :: Val -> Val -> TypeCheck Val

whnfClos :: Val -> TypeCheck Val
force :: Val -> TypeCheck Val
piApps :: TVal -> [Clos] -> TypeCheck TVal

matchList :: Env -> [Pattern] -> [Val] -> TypeCheck (Maybe Env)

type GenToPattern = [(Int,Pattern)]
type MatchState = (Env, GenToPattern)
nonLinMatchList' :: Bool -> Bool -> MatchState -> [Pattern] -> [Val] -> TVal -> TypeCheck (Maybe MatchState)

projectType :: TVal -> Name -> Val -> TypeCheck TVal

up    :: Bool -> Val -> TVal -> TypeCheck Val

leqSize' :: Val -> Val -> TypeCheck ()

mkConVal :: Dotted -> ConK -> QName -> [Val] -> TVal -> TypeCheck Val

{-# LANGUAGE TupleSections, FlexibleInstances, NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

-- Activate this flag if i < $i should only hold for i < #.
-- #define STRICTINFTY

module Eval where

import Prelude hiding (mapM, null, pi)

import Control.Applicative
import Control.Monad.Identity hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.IfElse  -- unlessM
-- import Control.Monad.HT      -- andLazy  -- because liftM2 (&&) is NOT lazy!

import qualified Data.Array as Array
import Data.Maybe -- fromMaybe
import Data.Monoid hiding ((<>))
import Data.List as List hiding (null) -- find
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldMap)
import Data.Traversable (Traversable, mapM, traverse)
import qualified Data.Traversable as Traversable

import Debug.Trace

import Abstract
import Polarity as Pol
import Value
import TCM
import PrettyTCM
import Warshall  -- positivity checking

import TraceError
import Util


traceEta msg a = a -- trace msg a
traceEtaM msg = return () -- traceM msg
{-
traceEta msg a = trace msg a
traceEtaM msg = traceM msg
-}

traceRecord msg a = a
traceRecordM msg = return ()


traceMatch msg a = a -- trace msg a
traceMatchM msg = return () -- traceM msg
{-
traceMatch msg a = trace msg a
traceMatchM msg = traceM msg
-}

traceLoop msg a = a -- trace msg a
traceLoopM msg = return () -- traceM msg
{-
traceLoop msg a = trace msg a
traceLoopM msg = traceM msg
-}

traceSize msg a = a -- trace msg a
traceSizeM msg = return () -- traceM msg
{-
traceSize msg a = trace msg a
traceSizeM msg = traceM msg
-}

-- evaluation with rewriting -------------------------------------

{-

Rewriting rules have the form

  blocked --> pattern

this means that at the root, at most one rewriting step is possible.
Rewriting rules are considered computational, since they trigger new
(symbolic) computations.  At least they have to be applied in

- pattern matching
- equality checking
When a new rule b --> p is added, b should be in --> normal form.
Otherwise there could be inconsistencies, like adding both rules

  b --> true
  b --> false

If after adding b --> true b is rewritten to nf, then the second rule
would be true --> false, which can be captured by MiniAgda.

Also, after adding a new rule, it could be used to rewrite the old rules.

Implementation:

- add a set of local rewriting rules to the context (not to the state)
- keep values in --> weak head normal form
- untyped equality test between values

 -}

class Reval a where
  reval' :: Valuation -> a -> TypeCheck a
  reval  :: a -> TypeCheck a
  reval = reval' emptyVal

instance Reval a => Reval (Maybe a) where
  reval' valu ma = Traversable.traverse (reval' valu) ma

instance Reval b => Reval (a,b) where
  reval' valu (x,v) = (x,) <$> reval' valu v

instance Reval a => Reval [a] where
  reval' valu vs = mapM (reval' valu) vs

instance Reval Env where
  reval' valu (Environ rho mmeas) =
   flip Environ mmeas <$> reval' valu rho
   -- no need to reevaluate mmeas, since only sizes

-- | When combining valuations, the old one takes priority.
--   @[sigma][tau]v = [[sigma]tau]v@
instance Reval Valuation where
  reval' valu (Valuation valu') = Valuation . (++ valuation valu) <$>
    reval' valu valu'

instance Reval a => Reval (Measure a) where
  reval' valu beta = Traversable.traverse (reval' valu) beta

instance Reval a => Reval (Bound a) where
  reval' valu beta = Traversable.traverse (reval' valu) beta

instance Reval Val where
  reval' valu u = traceLoop ("reval " ++ show u) $ do
    let reval v   = reval' valu v
        reEnv rho = reval' valu rho
        reFun fv  = reval' valu fv
    case u of
      VSort (CoSet v) -> VSort . CoSet <$> reval v
      VSort{} -> return u
      VInfty  -> return u
      VZero   -> return u
      VSucc{} -> return u  -- no rewriting in size expressions
      VMax{}  -> return u
      VPlus{}  -> return u
      VProj{}  -> return u -- cannot rewrite projection
      VPair v1 v2 -> VPair <$> reval v1 <*> reval v2
      VRecord ri rho -> VRecord ri <$> mapAssocM reval rho

      VApp v vl          -> do
        v'  <- reval v
        vl' <- mapM reval vl
        w   <- foldM app v' vl'
        reduce w  -- since we only have rewrite rules at base types
                  -- we do not need to reduces prefixes of w

      VDef{} -> return $ VApp u [] -- restore invariant
                                   -- CAN'T rewrite defined fun/data
      VGen i -> reduce (valuateGen i valu)  -- CAN rewrite variable

      VCase v tv env cl -> do
        v' <- reval v
        tv' <- reval tv
        env' <- reEnv env
        evalCase v' tv' env' cl

      VBelow ltle v         -> VBelow ltle <$> reval v
      VGuard beta v         -> VGuard <$> reval beta <*> reval v
      VQuant pisig x dom fv ->
        VQuant pisig x
          <$> Traversable.mapM reval dom
          <*> reFun fv
    {-
      VQuant pisig x dom env b -> do
        dom' <- Traversable.mapM reval dom
        env' <- reEnv env
        return $ VQuant pisig x dom' env' b
    -}
      VConst v           -> VConst <$> reval' valu v
      VLam x env e       -> flip (VLam x) e <$> reval' valu env
      VAbs x i v valu'   -> VAbs x i v <$> reval' valu valu'
      VUp v tv           -> up False ==<< (reval' valu v, reval' valu tv)  -- do not force at this point

      VClos env e        -> do env' <- reEnv env
                               return $ VClos env' e

      VMeta i env k      -> do env' <- reEnv env
                               return $ VMeta i env' k

      VSing v tv         -> vSing ==<< (reval v, reval tv)
      VIrr -> return u
      v -> throwErrorMsg $ "NYI : reval " ++ show v


-- TODO: singleton Sigma types
-- <t : Pi x:a.f> = Pi x:a <t x : f x>
-- <t : A -> B  > = Pi x:A <t x : B>
-- <t : <t' : a>> = <t' : a>
vSing :: Val -> TVal -> TypeCheck TVal
vSing v (VQuant Pi x' dom fv) = do
  let x = fresh $ if emptyName x' then "xSing#" else suggestion x'
  VQuant Pi x dom <$> do
  underAbs_ x dom fv $ \ i xv bv -> do
    v <- app v xv
    vAbs x i <$> vSing v bv
vSing _ tv@(VSing{}) = return $ tv
vSing v tv           = return $ VSing v tv
{-
-- This is a bit of a hack (finding a fresh name)
-- <t : Pi x:a.b> = Pi x:a <t x : b>
-- <t : Pi x:a.f> = Pi x:a <t x : f x>
-- <t : <t' : a>> = <t' : a>
vSing :: Val -> TVal -> TVal
vSing v (VQuant Pi x dom env b)
  | not (emptyName x) = -- xv `seq` x' `seq`
     (VQuant Pi x dom (update env xv v) $ Sing (App (Var xv) (Var x)) b)
      where xv = fresh ("vSing#" ++ suggestion x)
vSing v (VQuant Pi x dom env b) =
--  | otherwise =
     (VQuant Pi x' dom (update env xv v) $ Sing (App (Var xv) (Var x')) b')
      where xv = fresh ("vSing#" ++ suggestion x)
            x' = fresh $ if emptyName x then "xSing#" else suggestion x
            b' = parSubst (\ y -> Var $ if y == x then x' else y) b
vSing _ tv@(VSing{}) = tv
vSing v tv           = VSing v tv
-}

-- reduce the root of a value
reduce :: Val -> TypeCheck Val
reduce v = traceLoop ("reduce " ++ show v) $
 do
  rewrules <- asks rewrites
  mr <- findM (\ rr -> equal v (lhs rr)) rewrules
  case mr of
     Nothing -> return v
     Just rr -> traceRew ("firing " ++ show rr) $ return (rhs rr)

-- equal v v'  tests values for untyped equality
-- precond: v v' are in --> whnf
equal :: Val -> Val -> TypeCheck Bool
equal u1 u2 = traceLoop ("equal " ++ show u1 ++ " =?= " ++ show u2) $
  case (u1,u2) of
    (v1,v2) | v1 == v2 -> return True -- includes all size expressions
--    (VSucc v1, VSucc v2) -> equal v1 v2  -- NO REDUCING NECC. HERE (Size expr)
    (VApp v1 vl1, VApp v2 vl2) ->
       (equal v1 v2) `andLazy` (equals' vl1 vl2)
    (VQuant pisig1 x1 dom1 fv1, VQuant pisig2 x2 dom2 fv2) | pisig1 == pisig2 ->
       andLazy (equal (typ dom1) (typ dom2)) $  -- NO RED. NECC. (Type)
         new x1 dom1 $ \ vx -> equal ==<< (app fv1 vx, app fv2 vx)
    (VProj _ p, VProj _ q) -> return $ p == q
    (VPair v1 w1, VPair v2 w2) -> (equal v1 v2) `andLazy` (equal w1 w2)
    (VBelow ltle1 v1, VBelow ltle2 v2) | ltle1 == ltle2 -> equal v1 v2
    (VSing v1 tv1, VSing v2 tv2) -> (equal v1 v2) `andLazy` (equal tv1 tv2)

    (fv1, fv2) | isFun fv1, isFun fv2 -> -- PROBLEM: DOM. MISSING, CAN'T "up" fresh variable
      addName (bestName [absName fv1, absName fv2]) $ \ vx ->
        equal ==<< (app fv1 vx, app fv2 vx)
{-
    (VLam x1 env1 b1, VLam x2 env2 b2) -> -- PROBLEM: DOMAIN MISSING
         addName x1 $ \ vx -> do          -- CAN'T "up" fresh variable
               do v1 <- whnf (update env1 x1 vx) b1
                  v2 <- whnf (update env2 x2 vx) b2
                  equal v1 v2
-}
    (VRecord ri1 rho1, VRecord ri2 rho2) | notDifferentNames ri1 ri2 -> and <$>
      zipWithM (\ (n1,v1) (n2,v2) -> ((n1 == n2) &&) <$> equal' v1 v2) rho1 rho2
    _ -> return False

notDifferentNames :: RecInfo -> RecInfo -> Bool
notDifferentNames (NamedRec _ n _ _) (NamedRec _ n' _ _) = n == n'
notDifferentNames _ _ = True

equals' :: [Val] -> [Val] -> TypeCheck Bool
equals' [] []             = return True
equals' (w1:vs1) (w2:vs2) = (equal' w1 w2) `andLazy` (equals' vs1 vs2)
equals' vl1 vl2           = return False

equal' w1 w2 = whnfClos w1 >>= \ v1 -> equal v1 =<< whnfClos w2

{- LEADS TO NON-TERMINATION
-- equal' v1 v2  tests values for untyped equality
-- v1 v2 are not necessarily in --> whnf
equal' v1 v2 = do
  v1' <- reduce v1
  v2' <- reduce v2
  equal v1' v2'
-}

-- normalization -----------------------------------------------------

reify :: Val -> TypeCheck Expr
reify v = reify' (5, True) v

-- normalize to depth m
reify' :: (Int, Bool) -> Val -> TypeCheck Expr
reify' m v0 = do
  let reify = reify' m  -- default recursive call
  case v0 of
    (VClos rho e)        -> whnf rho e >>= reify
    (VZero)              -> return $ Zero
    (VInfty)             -> return $ Infty
    (VSucc v)            -> Succ <$> reify v
    (VMax vs)            -> maxE <$> mapM reify vs
    (VPlus vs)           -> Plus <$> mapM reify vs
    (VMeta x rho n)      -> -- error $ "cannot reify meta-variable " ++ show v0
                            return $ iterate Succ (Meta x) !! n
    (VSort (CoSet v))    -> Sort . CoSet <$> reify v
    (VSort s)            -> return $ Sort $ vSortToSort s
    (VBelow ltle v)      -> Below ltle <$> reify v
    (VQuant pisig x dom fv) -> do
          dom' <- Traversable.mapM reify dom
          underAbs_ x dom fv $ \ k xv vb -> do
            let x' = unsafeName (suggestion x ++ "~" ++ show k)
            piSig pisig (TBind x' dom') <$> reify vb
    (VSing v tv)         -> liftM2 Sing (reify v) (reify tv)
    fv | isFun fv        -> do
          let x = absName fv
          addName x $ \ xv@(VGen k) -> do
            vb <- app fv xv
            let x' = unsafeName (suggestion x ++ "~" ++ show k)
            Lam defaultDec x' <$> reify vb  -- TODO: dec!?
    (VUp v tv)           -> reify v -- TODO: type directed reification
    (VGen k)             -> return $ Var $ unsafeName $ "~" ++ show k
    (VDef d)             -> return $ Def d
    (VProj fx n)         -> return $ Proj fx n
    (VPair v1 v2)        -> Pair <$> reify v1 <*> reify v2
    (VRecord ri rho)     -> Record ri <$> mapAssocM reify rho
    (VApp v vl)          -> if fst m > 0 && snd m
                             then force v0 >>= reify' (fst m - 1, True) -- forgotten the meaning of the boolean, WAS: False)
                             else let m' = (fst m, True) in
                               liftM2 (foldl App) (reify' m' v) (mapM (reify' m') vl)
    (VCase v tv rho cls)    -> do
          e <- reify v
          t <- reify tv
          return $ Case e (Just t) cls -- TODO: properly evaluate clauses!!
    (VIrr)               -> return $ Irr
    v -> failDoc (text "Eval.reify" <+> prettyTCM v <+> text "not implemented")

-- printing (conversion to Expr) -------------------------------------

-- similar to reify
toExpr :: Val -> TypeCheck Expr
toExpr v =
  case v of
    VClos rho e     -> closToExpr rho e
    VZero           -> return $ Zero
    VInfty          -> return $ Infty
    (VSucc v)       -> Succ <$> toExpr v
    VMax vs         -> maxE <$> mapM toExpr vs
    VPlus vs        -> Plus <$> mapM toExpr vs
    VMeta x rho n   -> metaToExpr x rho n
    VSort s         -> Sort <$> mapM toExpr s
{-
    VSort (CoSet v) -> (Sort . CoSet) <$> toExpr v
    VSort (Set v)   -> (Sort . Set) <$> toExpr v
    VSort (SortC s) -> return $ Sort (SortC s)
-}
    VMeasured mu bv -> pi <$> (TMeasure <$> mapM toExpr mu) <*> toExpr bv
    VGuard beta bv  -> pi <$> (TBound <$> mapM toExpr beta) <*> toExpr bv
    VBelow Le VInfty -> return $ Sort $ SortC Size
    VBelow ltle bv  -> Below ltle <$> toExpr bv
    VQuant pisig x dom fv -> underAbs' x fv $ \ xv bv ->
      piSig pisig <$> (TBind x <$> mapM toExpr dom) <*> toExpr bv
    VSing v tv      -> Sing <$> toExpr v <*> toExpr tv
    fv | isFun fv   -> addName (absName fv) $ \ xv -> toExpr =<< app fv xv
{-
    VLam x rho e    -> addNameEnv x rho $ \ x rho ->
      Lam defaultDec x <$> closToExpr rho e
-}
    VUp v tv        -> toExpr v
    VGen k          -> Var <$> nameOfGen k
    VDef d          -> return $ Def d
    VProj fx n      -> return $ Proj fx n
    VPair v1 v2     -> Pair <$> toExpr v1 <*> toExpr v2
    VRecord ri rho  -> Record ri <$> mapAssocM toExpr rho
    VApp v vl       -> liftM2 (foldl App) (toExpr v) (mapM toExpr vl)
    VCase v tv rho cls -> Case <$> toExpr v <*> (Just <$> toExpr tv) <*> mapM (clauseToExpr rho) cls
    VIrr            -> return $ Irr

{-
addBindEnv :: TBind -> Env -> (Env -> TypeCheck a) -> TypeCheck a
addBindEnv (TBind x dom) rho cont = do
  let dom' = fmap (VClos rho) dom
  newWithGen x dom' $ \ k _ ->
    cont (update rho x (VGen k))
-}

addNameEnv :: Name -> Env -> (Name -> Env -> TypeCheck a) -> TypeCheck a
--addNameEnv "" rho cont = cont "" rho
addNameEnv x rho cont = do
  let dom' = defaultDomain VIrr -- error $ "internal error: variable " ++ show x ++ " comes without domain"
  newWithGen x dom' $ \ k _ -> do
    x' <- nameOfGen k
    cont x' (update rho x (VGen k))

addPatternEnv :: Pattern -> Env -> (Pattern -> Env -> TypeCheck a) -> TypeCheck a
addPatternEnv p rho cont =
  case p of
    VarP x       -> addNameEnv     x  rho $ cont . VarP -- \ x rho -> cont (VarP x) rho
    SizeP e x    -> addNameEnv     x  rho $ cont . VarP
    PairP p1 p2  -> addPatternEnv  p1 rho $ \ p1 rho ->
                     addPatternEnv p2 rho $ \ p2 rho -> cont (PairP p1 p2) rho
    ConP pi n ps -> addPatternsEnv ps rho $ cont . ConP pi n -- \ ps rho -> cont (ConP pi n ps) rho
    SuccP p      -> addPatternEnv  p  rho $ cont . SuccP
    UnusableP p  -> addPatternEnv  p  rho $ cont . UnusableP
    DotP e       -> do { e <- closToExpr rho e ; cont (DotP e) rho }
    AbsurdP      -> cont AbsurdP rho
    ErasedP p    -> addPatternEnv  p  rho $ cont . ErasedP

addPatternsEnv :: [Pattern] -> Env -> ([Pattern] -> Env -> TypeCheck a) -> TypeCheck a
addPatternsEnv []     rho cont = cont [] rho
addPatternsEnv (p:ps) rho cont =
  addPatternEnv p rho $ \ p rho ->
    addPatternsEnv ps rho $ \ ps rho ->
      cont (p:ps) rho

{-
class BindClosToExpr a where
  bindClosToExpr :: Env -> a -> (Env -> a -> TCM b) -> TCM b

instance ClosToExpr a => BindClosToExpr (TBinding a) where
  bindClosToExpr
-}

class ClosToExpr a where
  closToExpr     :: Env -> a -> TypeCheck a
  bindClosToExpr :: Env -> a -> (Env -> a -> TypeCheck b) -> TypeCheck b

  -- default : no binding
  closToExpr rho a = bindClosToExpr rho a $ \ rho a -> return a
  bindClosToExpr rho a cont = cont rho =<< closToExpr rho a

instance ClosToExpr a => ClosToExpr [a] where
  closToExpr = traverse . closToExpr

instance ClosToExpr a => ClosToExpr (Maybe a) where
  closToExpr = traverse . closToExpr

instance ClosToExpr a => ClosToExpr (Dom a) where
  closToExpr = traverse . closToExpr

instance ClosToExpr a => ClosToExpr (Sort a) where
  closToExpr = traverse . closToExpr

instance ClosToExpr a => ClosToExpr (Measure a) where
  closToExpr = traverse . closToExpr

instance ClosToExpr a => ClosToExpr (Bound a) where
  closToExpr = traverse . closToExpr

instance ClosToExpr a => ClosToExpr (Tagged a) where
  closToExpr = traverse . closToExpr

instance ClosToExpr a => ClosToExpr (TBinding a) where
  bindClosToExpr rho (TBind x a) cont = do
    a <- closToExpr rho a
    addNameEnv x rho $ \ x rho -> cont rho $ TBind x a
  bindClosToExpr rho (TMeasure mu) cont = cont rho . TMeasure =<< closToExpr rho mu
  bindClosToExpr rho (TBound beta) cont = cont rho . TBound =<< closToExpr rho beta

instance ClosToExpr Telescope where
  bindClosToExpr rho (Telescope tel) cont = loop rho tel $ \ rho -> cont rho . Telescope
    where
      loop rho []         cont = cont rho []
      loop rho (tb : tel) cont = bindClosToExpr rho tb $ \ rho tb ->
        loop rho tel $ \ rho tel -> cont rho $ tb : tel

instance ClosToExpr Expr where
  closToExpr rho e =
    case e of
      Sort s         -> Sort <$> closToExpr rho s
      Zero           -> return e
      Succ e         -> Succ <$> closToExpr rho e
      Infty          -> return e
      Max es         -> Max  <$> closToExpr rho es
      Plus es        -> Plus <$> closToExpr rho es
      Meta x         -> return e
      Var x          -> toExpr =<< whnf rho e
      Def d          -> return e
      Case e mt cls  -> Case <$> closToExpr rho e <*> closToExpr rho mt <*> mapM (clauseToExpr rho) cls
      LLet tb tel e1 e2 | null tel -> do
        e1 <- closToExpr rho e1
        bindClosToExpr rho tb $ \ rho tb -> LLet tb tel e1 <$> closToExpr rho e2
      Proj fx n      -> return e
      Record ri rs   -> Record ri <$> mapAssocM (closToExpr rho) rs
      Pair e1 e2     -> Pair <$> closToExpr rho e1 <*> closToExpr rho e2
      App e1 e2      -> App <$> closToExpr rho e1 <*> closToExpr rho e2
      Lam dec x e    -> addNameEnv x rho $ \ x rho ->
        Lam dec x <$> closToExpr rho e
      Below ltle e   -> Below ltle <$> closToExpr rho e
{-
      Quant Pi tel mu@TMeasure{} e | null tel -> pi <$> closToExpr rho mu   <*> closToExpr rho e
      Quant Pi tel beta@TBound{} e | null tel -> pi <$> closToExpr rho beta <*> closToExpr rho e
-}
      Quant piSig tb e -> bindClosToExpr rho tb $ \ rho tb -> Quant piSig tb <$> closToExpr rho e
--       Quant piSig tel tb e -> bindClosToExpr rho tel $ \ rho tel ->
--         bindClosToExpr rho tb $ \ rho tb -> Quant piSig tel tb <$> closToExpr rho e
      Sing e1 e2     -> Sing <$> closToExpr rho e1 <*> closToExpr rho e2
      Ann taggedE    -> Ann <$> closToExpr rho taggedE
      Irr            -> return e

metaToExpr :: Int -> Env -> Int -> TypeCheck Expr
metaToExpr x rho k = return $ iterate Succ (Meta x) !! k

clauseToExpr :: Env -> Clause -> TypeCheck Clause
clauseToExpr rho (Clause vtel ps me) = addPatternsEnv ps rho $ \ ps rho ->
   Clause vtel ps <$> mapM (closToExpr rho) me

-- evaluation --------------------------------------------------------

-- | Weak head normal form.
--   Monadic, since it reads the globally defined constants from the signature.
--   @let@s are expanded away.

whnf :: Env -> Expr -> TypeCheck Val
whnf env e = enter ("whnf " ++ show e) $
  case e of
    Meta i -> do let v = VMeta i env 0
                 traceMetaM $ "whnf meta " ++ show v
                 return v
    LLet (TBind x dom) tel e1 e2 | null tel -> do
      let v1 = mkClos env e1
      whnf (update env x v1) e2
{-
-- ALT: remove erased lambdas entirely
    Lam dec x e1 | erased dec -> whnf env e1
                 | otherwise -> return $ VLam x env e1
-}
    Lam dec x e1 -> return $ vLam x env e1
    Below ltle e -> VBelow ltle <$> whnf env e
    Quant pisig (TBind x dom) b -> do
      dom' <- Traversable.mapM (whnf env) dom  -- Pi is strict in its first argument
      return $ VQuant pisig x dom' $ vLam x env b

    -- a measured type evaluates to
    -- * a bounded type if measure present in environment (rhs of funs)
    -- * otherwise to a measured type (lhs of funs)
    Quant Pi (TMeasure mu) b -> do
      muv <- whnfMeasure env mu
      bv  <- whnf env b -- not adding measure constraint to context!
      case (envBound env) of
        Nothing   -> return $ VMeasured muv bv
           -- fail $ "panic: whnf " ++ show e ++ " : no measure in environment " ++ show env
        Just muv' -> return $ VGuard (Bound Lt muv muv') bv

    Quant Pi (TBound (Bound ltle mu mu')) b -> do
          muv  <- whnfMeasure env mu
          muv' <- whnfMeasure env mu'
          bv   <- whnf env b  -- not adding measure constraint to context!
          return $ VGuard (Bound ltle muv muv') bv

    Sing e t  -> do tv <- whnf env t
                    sing env e tv

    Pair e1 e2 -> VPair <$> whnf env e1 <*> whnf env e2
    Proj fx n  -> return $ VProj fx n

    Record ri@(NamedRec Cons _ _ _) rs -> VRecord ri <$> mapAssocM (whnf env) rs

    -- coinductive and anonymous records are treated lazily:
    Record ri rs -> return $ VRecord ri $ mapAssoc (mkClos env) rs

{-
-- ALT: filter out all erased arguments from application
    App e1 el -> do v1 <- whnf env e1
                    vl <- liftM (filter (/= VIrr)) $ mapM (whnf env) el
                    app v1 vl
-}
    App f e   -> do vf <- whnf env f
                    let ve = mkClos env e
                    app vf ve
{-
    App e1 el -> do v1 <- whnf env e1
                    vl <- mapM (whnf env) el
                    app v1 vl
-}

    Case e (Just t) cs -> do
      v  <- whnf env e
      vt <- whnf env t
      evalCase v vt env cs
                  -- trace ("case head evaluates to " ++ showVal v) $ return ()

    Sort s -> whnfSort env s >>= return . vSort
    Infty -> return VInfty
    Zero -> return VZero
    Succ e1 -> do v <- whnf env e1           -- succ is strict
                  return $ succSize v

    Max es  -> do vs <- mapM (whnf env) es   -- max is strict
                  return $ maxSize vs
    Plus es -> do vs <- mapM (whnf env) es   -- plus is strict
                  return $ plusSizes vs

    Def (DefId LetK n) -> do
        item <- lookupSymbQ n
        whnfClos (definingVal item)

    Def (DefId (ConK DefPat) n) -> whnfClos . definingVal =<< lookupSymbQ n
--    Def (DefId (ConK DefPat) n) -> fail $ "internal error: whnf of defined pattern " ++ show n
    Def id   -> return $ vDef id
{-
    Con co n -> return $ VCon co n

    Def n -> return $ VDef n

    Let n -> do sig <- gets signature
                let (LetSig _ v) = lookupSig n sig
                return v
--                let (LetSig _ e) = lookupSig n sig
--                whnf [] e
-}
    Var y -> lookupEnv env y >>= whnfClos
    Ann e -> whnf env (unTag e) -- return VIrr -- NEED TO KEEP because of eta-exp!
    Irr -> return VIrr
    e   -> fail $ "NYI whnf " ++ show e

whnfMeasure :: Env -> Measure Expr -> TypeCheck (Measure Val)
whnfMeasure rho (Measure mu) = mapM (whnf rho) mu >>= return . Measure

whnfSort :: Env -> Sort Expr -> TypeCheck (Sort Val)
whnfSort rho (SortC c) = return $ SortC c
whnfSort rho (CoSet e) = whnf rho e >>= return . CoSet
whnfSort rho (Set e)   = whnf rho e >>= return . Set

whnfClos :: Clos -> TypeCheck Val
whnfClos v = -- trace ("whnfClos " ++ show v) $
  case v of
    (VClos e rho) -> whnf e rho
    -- (VApp (VProj Pre n) [u]) -> app u (VProj Post n) -- NO EFFECT
    (VApp (VDef (DefId FunK n)) vl) -> appDef n vl -- THIS IS TO SOLVE A PROBLEM
    v -> return v
{- THE PROBLEM IS that
  (tail (x Up Stream)) Up Stream is a whnf, because Up Stream is lazy
  in equality checking this is a problem when the Up is removed.
-}

-- evaluate in standard environment
whnf' :: Expr -> TypeCheck Val
whnf' e = do
  env <- getEnv
  whnf env e

-- <t : Pi x:a.b> = Pi x:a <t x : b>
-- <t : <t' : a>> = <t' : a>
sing :: Env -> Expr -> TVal -> TypeCheck TVal
sing rho e tv = do
  let v = mkClos rho e -- v <- whnf rho e
  vSing v tv
{-
sing env' e (VPi dec x av env b)  = do
  return $ VPi dec x' av env'' (Sing (App e (Var x')) b)
    where env'' = env' ++ env  -- super ugly HACK
          x'    = if x == "" then fresh env'' else x
    -- Should work with just x since shadowing is forbidden
sing _ _ tv@(VSing{}) = return $ tv
sing env e tv = do v <- whnf env e      -- singleton strict, is this OK?!
                   return $ VSing v tv
-}

sing' :: Expr -> TVal -> TypeCheck TVal
sing' e tv = do
  env <- getEnv
  sing env e tv

evalCase :: Val -> TVal -> Env -> [Clause] -> TypeCheck Val
evalCase v tv env cs = do
  m  <- matchClauses env cs [v]
  case m of
    Nothing -> return $ VCase v tv env cs
    Just v' -> return $ v'

piApp :: TVal -> Clos -> TypeCheck TVal
piApp (VGuard beta bv) w = piApp bv w
piApp (VQuant Pi x dom fv) w = app fv w
piApp tv@(VApp (VDef (DefId DatK n)) vl) (VProj Post p) = projectType tv p VIrr -- no rec value here
piApp tv w = failDoc (text "piApp: IMPOSSIBLE to instantiate" <+> prettyTCM tv <+> text "to argument" <+> prettyTCM w)

piApps :: TVal -> [Clos] -> TypeCheck TVal
piApps tv [] = return tv
piApps tv (v:vs) = do tv' <- piApp tv v
                      piApps tv' vs

updateValu valu i v = reval' (sgVal i v) valu

-- in app u v, u might be a VDef (e.g. when coming from reval)
app :: Val -> Clos -> TypeCheck Val
app = app' True

-- | Application of arguments and projections.
app' :: Bool -> Val -> Clos -> TypeCheck Val
app' expandDefs u v = do
         let app = app' expandDefs
             appDef' True  f vs = appDef f vs
             appDef' False f vs = return $ VDef (DefId FunK f) `VApp` vs
             appDef_ = appDef' expandDefs
         case u of
            VProj Pre n -> flip (app' expandDefs) (VProj Post n) =<< whnfClos v
            VRecord ri rho -> do
              let VProj Post n = v
              maybe (fail $ "app: projection " ++ show n ++ " not found in " ++ show u)
                whnfClos (lookup n rho)
            VDef (DefId FunK n) -> appDef_ n [v]
            VApp (VDef (DefId FunK n)) vl -> appDef_ n (vl ++ [v])
            VApp h@(VDef (DefId (ConK Cons) n)) vl -> do
              v <- whnfClos v      -- inductive constructors are strict!
              return $ VApp h (vl ++ [v])
--            VDef n -> appDef n [v]
--            VApp (VDef id) vl -> VApp (VDef id) (vl ++ [v])
            VApp v1 vl -> return $ VApp v1 (vl ++ [v])

-- VSing is a type!
--           VSing u (VQuant Pi x dom fu) -> vSing <$> app u v <*> app fu v

            VLam x env e    -> whnf (update env x v) e
            VConst u        -> whnfClos u
            VAbs x i u valu -> flip reval' u =<< updateValu valu i v
            VUp u (VQuant Pi x dom fu) -> up False ==<< (app u v, app fu v)

{-
            VUp u1 (VQuant Pi x dom rho b) -> do
{-
-- ALT: erased functions are not applied to their argument!
              v1 <- if erased dec then return v else app v [w]  -- eta-expand w ??
-}
              v1 <- app u1 v  -- eta-expand v ??
              bv <- whnf (update rho x v) b
              up False v1 bv
-}
            VUp u1 (VApp (VDef (DefId DatK n)) vl) -> do
              u' <- force u
              app u' v

            VIrr -> return VIrr
{- 2010-11-01 this breaks extraction for System U example
            VIrr -> fail $ "app internal error: " ++ show (VApp u [v])
-}
            _ -> return $ VApp u [v]
--
-- app :: Val -> [Val] -> TypeCheck Val
-- app u [] = return $ u
-- app u c = do
--          case (u,c) of
--             (VApp u2 c2,_) -> app u2 (c2 ++ c)
--             (VLam x env e,(v:vl))  -> do v' <- whnf (update env x v) e
--                                          app v' vl
--             (VDef n,_) -> appDef n c
--             (VUp v (VPi dec x av rho b), w:wl) -> do
-- {-
-- -- ALT: erased functions are not applied to their argument!
--               v1 <- if erased dec then return v else app v [w]  -- eta-expand w ??
-- -}
--               v1 <- app v [w]  -- eta-expand w ??
--               bv <- whnf (update rho x w) b
--               v2 <- up v1 bv
--               app v2 wl
-- {-
-- -- ALT: VIrr consumes applications
--             (VIrr,_) -> return VIrr
--  -}
--             (VIrr,_) -> fail $ "app internal error: " ++ show (VApp u c)
--             _ -> return $ VApp u c


-- unroll a corecursive definition one time (until constructor appears)
force' :: Bool -> Val -> TypeCheck (Bool, Val)
force' b (VSing v tv) = do  -- for singleton types, force type!
  (b',tv') <- force' b tv
  return (b', VSing v tv')
force' b (VUp v tv) = up True v tv >>= \ v' -> return (True, v')  -- force eta expansion
force' b (VClos rho e) = do
  v <- whnf rho e
  force' b v
force' b v@(VDef (DefId FunK n)) = failValInv v
{-
 --trace ("force " ++ show v) $
    do sig <- gets signature
       case lookupSig n sig of
         (FunSig CoInd t cl True) -> do m <- matchClauses [] cl []
                                        case m of
                                          Just v' -> force v'
                                          Nothing -> return v
         _ -> return v
-}
force' b v@(VApp (VDef (DefId FunK n)) vl) = enterDoc (text "force" <+> prettyTCM v) $
    do sig <- gets signature
       case Map.lookup n sig of
         Just (FunSig isCo t ki ar cl True _) -> traceMatch ("forcing " ++ show v) $
            do m <- matchClauses emptyEnv cl vl
               case m of
                 Just v' -> traceMatch ("forcing " ++ show n ++ " succeeded") $
                   force' True v'
                 Nothing -> traceMatch ("forcing " ++ show n ++ " failed") $
                   return (b, v)
         _ -> return (b, v)
force' b v = return (b, v)

force :: Val -> TypeCheck Val
force v = -- trace ("forcing " ++ show v) $
  liftM snd $ force' False v

-- apply a recursive function
-- corecursive ones are not expanded even if the arity is exceeded
-- this is because a coinductive type needs to be destructed by pattern matching
appDef :: QName -> [Val] -> TypeCheck Val
appDef n vl = --trace ("appDef " ++ n) $
    do
      -- identifier might not be in signature yet, e.g. ind.-rec.def.
      sig <- gets signature
      case (Map.lookup n sig) of
        Just (FunSig { isCo = Ind, arity = ar, clauses = cl, isTypeChecked = True })
         | length vl >= fullArity ar -> do
           m <- matchClauses emptyEnv cl vl
           case m of
              Nothing -> return $ VApp (VDef (DefId FunK n)) vl
              Just v2 -> return v2
        _ -> return $ VApp (VDef (DefId FunK n)) vl

-- reflection and reification  ---------------------------------------

-- TODO: eta for builtin sigma-types !?

-- up force v tv
-- force==True also expands at coinductive type
up :: Bool -> Val -> TVal -> TypeCheck Val
up f (VUp v tv') tv                              = up f v tv
up f v           tv@VQuant{ vqPiSig = Pi }       = return $ VUp v tv
up f _           (VSing v vt)                    = up f v vt
up f v           (VDef d)                        = failValInv $ VDef d
up f v           (VApp (VDef (DefId DatK d)) vl) = upData f v d vl
up f v           _                               = return v

{- Most of the code to eta expand on data types is in
   TypeChecker.hs "typeCheckDeclaration"

 Currently, eta expansion only happens at data *types* with exactly
one constructor.  In a first step, this will be extended to
non-recursive pattern inductive families.

The strategy is: match type value with result type for all the constructors
0. if there are no matches, eta expand to * (VIrr)
1. if there is exactly one match, eta expand accordingly using destructors
2. if there are more matches, do not eta-expand

up{Vec A (suc n)} x = vcons A n (head A n x) (tail A n x)

up{Vec Bool (suc zero)} x
  = vcons Bool zero (head Bool zero x) (tail Bool zero x)

For vcons
- the patterns of  Vec : (A : Set) -> Nat -> Set  are  [A,suc n]
- matching  Bool,suc zero  against  A,suc n  yields A=Bool,n=zero
- this means we can eta expand to vcons
- go through the fields of vcons
  - if Index use value obtained by matching
  - if Field destr, use  destr <all pars> <all indices> x

-}

-- matchingConstructors is for use in checkPattern
-- matchingConstructors (D vs)  returns all the constructors
-- each as tuple (ci,rho)
-- of family D whose target matches (D vs) under substitution rho
matchingConstructors :: Val -> TypeCheck (Maybe [(ConstructorInfo,Env)])
matchingConstructors v@(VDef d) = failValInv v -- matchingConstructors' d []
matchingConstructors (VApp (VDef (DefId DatK d)) vl) = matchingConstructors' d vl >>= return . Just
matchingConstructors v = return Nothing
-- fail $ "matchingConstructors: not a data type: " ++ show v -- return []

matchingConstructors' :: QName -> [Val] -> TypeCheck [(ConstructorInfo,Env)]
matchingConstructors' n vl = do
  sige <- lookupSymbQ n
  case sige of
    (DataSig {symbTyp = dv, constructors = cs}) -> -- if (null cs) then ret [] else do -- no constructor
      matchingConstructors'' True vl dv cs

-- matchingConstructors''
-- Arguments:
--   symm     symmetric match
--   vl       arguments to D (instance of D)
--   dv       complete type value of D
--   cs       constructors
-- Returns a list [(ci,rho)] of matching constructors together with the
--   environments which are solutions for the free variables in the constr.type
-- this is also for use in upData
matchingConstructors'' :: Bool -> [Val] -> Val -> [ConstructorInfo] -> TypeCheck [(ConstructorInfo,Env)]
matchingConstructors'' symm vl dv cs = do
  vl <- mapM whnfClos vl
  compressMaybes <$> do
    forM cs $ \ ci -> do
      let ps = snd (cPatFam ci)
          -- list of patterns ps where D ps is the constructor target
      fmap (ci,) <$> nonLinMatchList symm emptyEnv ps vl dv


data MatchingConstructors a
  = NoConstructor
  | OneConstructor a
  | ManyConstructors
  | UnknownConstructors
    deriving (Eq,Show)

getMatchingConstructor
  :: Bool           -- eta   : must the field etaExpand be set of the data type
  -> QName          -- d     : the name of the data types
  -> [Val]          -- vl    : the arguments of the data type
  -> TypeCheck (MatchingConstructors
     ( Co           -- co    : coinductive type?
     , [Val]        -- parvs : the parameter half of the arguments
     , Env          -- rho   : the substitution for the index variables to arrive at d vl
     , [Val]        -- indvs : the index values of the constructor
     , ConstructorInfo -- ci : the only matching constructor
     ))
getMatchingConstructor eta n vl = traceRecord ("getMatchingConstructor " ++ show (n, vl)) $
 do
  -- when checking a mutual data decl, the sig entry of the second data
  -- is not yet in place when checking the first, thus, lookup may fail
  sig <- gets signature
  case Map.lookup n sig of
    Just (DataSig {symbTyp = dv, numPars = npars, isCo = co, constructors = cs, etaExpand}) | eta `implies` etaExpand ->
     if (null cs) then return NoConstructor else do -- no constructor: empty type
       -- for each constructor, match its core against the type
      -- produces a list of maybe (c.info, environment)
      cenvs <- matchingConstructors'' False vl dv cs
      traceRecordM $ "Matching constructors: " ++ show cenvs
      case cenvs of
        -- exactly one matching constructor: can eta expand
--        [(ci,env)] -> if not (eta `implies` cEtaExp ci) then return UnknownConstructors else do
        [(ci,env)] -> if eta && not (cEtaExp ci) then return UnknownConstructors else do
          -- get list of index values from environment
          let fis = cFields ci
          let indices = filter (\ fi -> fClass fi == Index) fis
          let indvs = map (\ fi -> lookupPure env (fName fi)) indices
          let (pars, _) = splitAt npars vl
          return $ OneConstructor (co, pars, env, indvs, ci)
        -- more or less than one matching constructors: cannot eta expand
        l -> -- trace ("getMatchingConstructor: " ++ show (length l) ++ " patterns match at type " ++ show n ++ show vl) $
               return ManyConstructors
    _ -> traceRecord ("no eta expandable type") $ return UnknownConstructors

getFieldsAtType
  :: QName          -- d     : the name of the data types
  -> [Val]          -- vl    : the arguments of the data type
  -> TypeCheck
     (Maybe         -- Nothing if not a record type
       [(Name       -- list of projection names
        ,TVal)])    -- and their instantiated type R ... -> C
getFieldsAtType n vl = do
  mc <- getMatchingConstructor False n vl
  case mc of
    OneConstructor (_, pars, _, indvs, ci) -> do
      let pi = pars ++ indvs
      -- for each argument of constructor, get value
      let arg (FieldInfo { fName = x, fClass = Index }) = return []
          arg (FieldInfo { fName = d, fClass = Field _ }) = do
            -- lookup type sig  t  of destructor  d
            t <- lookupSymbTyp d
            -- pi-apply destructor type to parameters and indices
            t' <- piApps t pi
            return [(d,t')]
      Just . concat <$> mapM arg (cFields ci)
    _ -> return Nothing

-- similar to piApp, but for record types and projections
projectType :: TVal -> Name -> Val -> TypeCheck TVal
projectType tv p rv = do
  let fail1 = failDoc (text "expected record type when taking the projection" <+> prettyTCM (Proj Post p) <> comma <+> text "but found type" <+> prettyTCM tv)
  let fail2 = failDoc (text "record type" <+> prettyTCM tv <+> text "does not have field" <+> prettyTCM p)
  case tv of
    VApp (VDef (DefId DatK d)) vl -> do
      mfs <- getFieldsAtType d vl
      case mfs of
        Nothing -> fail1
        Just ptvs ->
          case lookup p ptvs of
            Nothing -> fail2
            Just tv -> piApp tv rv -- apply to record arg
    _ -> fail1

-- eta expand  v  at data type  n vl
upData :: Bool -> Val -> QName -> [Val] -> TypeCheck Val
upData force v n vl = -- trace ("upData " ++ show v ++ " at " ++ n ++ show vl) $
 do
  let ret v' = traceEta ("Eta-expanding: " ++ show v ++ " --> " ++ show v' ++ " at type " ++ show n ++ show vl) $ return v'
  mc <- getMatchingConstructor True n vl
  case mc of
    NoConstructor -> ret VIrr
    OneConstructor (co, pars, env, indvs, ci) ->
      -- lazy eta-expansion for coinductive records like streams!
      if (co==CoInd && not force) then return $ VUp v (VApp (VDef $ DefId DatK n) vl) else do
          -- get list of index values from environment
          let fis = cFields ci
          let piv = pars ++ indvs ++ [v]
          -- for each argument of constructor, get value
          let arg (FieldInfo { fName = x, fClass = Index }) =
                lookupEnv env x
              arg (FieldInfo { fName = d, fClass = Field _ }) = do
                -- lookup type sig  t  of destructor  d
                LetSig {symbTyp = t, definingVal = w} <- lookupSymb d
                -- pi-apply destructor type to parameters, indices and value v
                t' <- piApps t piv
                -- recursively eta expand  (d <pars> v)
                -- OLD, defined projections:
                -- w <- foldM (app' False) w piv -- LAZY: only unfolds let, not def
                -- NEW, builtin projections:
                w <- app' False v (VProj Post d)
                up False w t' -- now: LAZY

          vs <- mapM arg fis
          let fs = map fName fis
              v' = VRecord (NamedRec (coToConK co) (cName ci) False notDotted) $ zip fs vs
--          v' <- foldM app (vCon (coToConK co) (cName ci)) vs -- 2012-01-22 PARS GONE: (pars ++ vs)
          ret v'
    -- more constructors or unknown situation: do not eta expand
    _ -> return v

{-
-- eta expand  v  at data type  n vl
upData :: Bool -> Val -> Name -> [Val] -> TypeCheck Val
upData force v n vl = -- trace ("upData " ++ show v ++ " at " ++ n ++ show vl) $
 do
  let ret v' = traceEta ("Eta-expanding: " ++ show v ++ " --> " ++ show v' ++ " at type " ++ n ++ show vl) $ return v'
  -- when checking a mutual data decl, the sig entry of the second data
  -- is not yet in place when checking the first, thus, lookup may fail
  sig <- gets signature
  case Map.lookup n sig of
    Just (DataSig {symbTyp = dv, numPars = npars, isCo = co, constructors = cs, etaExpand = True}) -> if (null cs) then ret VIrr else do -- no constructor: empty type
      let (pars, inds) = splitAt npars vl
      -- for each constructor, match its core against the type
      -- produces a list of maybe (c.info, environment)
      cenvs <- matchingConstructors'' False vl dv cs
      -- traceM $ "Matching constructors: " ++ show cenvs
      case cenvs of
        -- exactly one matching constructor: can eta expand
        [(ci,env)] -> if not (cEtaExp ci) then return v else
         if (co==CoInd && not force) then return $ VUp v (VApp (VDef $ DefId Dat n) vl) else do
          -- get list of index values from environment
          let fis = cFields ci
          let indices = filter (\ fi -> fClass fi == Index) fis
          let indvs = map (\ fi -> lookupPure env (fName fi)) indices
          let piv = pars ++ indvs ++ [v]
          -- for each argument of constructor, get value
          let arg (FieldInfo { fName = x, fClass = Index }) =
                lookupEnv env x
              arg (FieldInfo { fName = d, fClass = Field _ }) = do
                -- lookup type sig  t  of destructor  d
                t <- lookupSymbTyp d
                -- pi-apply destructor type to parameters, indices and value v
                t' <- piApps t piv
                -- recursively eta expand  (d <pars> v)
                -- WAS: up (VDef (DefId Fun d) `VApp` piv) t'
                up False (VDef (DefId Fun d) `VApp` piv) t' -- now: LAZY
          vs <- mapM arg fis
          v' <- foldM app (vCon co (cName ci)) (pars ++ vs)
          ret v'
        -- more or less than one matching constructors: cannot eta expand
        l -> -- trace ("Eta: " ++ show (length l) ++ " patterns match at type " ++ show n ++ show vl) $
               return v
    _ -> return v
-}

{-
      let matchC (c, ps, ds) =
            do menv <- nonLinMatchList [] ps inds dv
               case menv of
                 Nothing -> return False
                 Just env -> do
                   let grps = groupBy (\ (x,_) (y,_) -> x == y) env
                   -- TODO: now compare elements in the group
                   -- NEED types for equality check
                   -- trivial if groups are singletons
                   return $ all (\ l -> length l <= 1) grps
      cs' <- filterM matchC cs
      case cs' of
        [] -> return $ VIrr
        [(c,_,ds)] ->  do
          let parsv = pars ++ [v]
          let aux d = do
               -- lookup type sig  t  of destructor  d
               let FunSig { symbTyp = t } = lookupSig d sig
               -- pi-apply destructor type to parameters and value v
               t' <- piApps t parsv
               -- recursively eta expand  (d <pars> v)
               up (VDef d `VApp` parsv) t'
          vs <- mapM aux ds
          app (VCon co c) (pars ++ vs)
        _ -> return v
    _ -> return v
-}

{-
refl : [A : Set] -> [a : A] -> Id A a a
up{Id T t t'} x
  Id T t t' =?= Id A a a  --> A = T, a = t, a = t'
-}

{- OLD CODE FOR NON-DEPENDENT RECORDS ONLY
    -- erase if n is a empty type
    (DataSig {constructors = []}) -> return $ VIrr
    -- eta expand v if n is a tuple type
    (DataSig {isCo = co, constructors = [c], destructors = Just ds}) -> do
       let vlv = vl ++ [v]
       let aux d = do -- lookup type sig  t  of destructor  d
                      let FunSig { symbTyp = t } = lookupSig d sig
                      -- pi-apply destructor type to parameters and value v
                      t' <- piApps t vlv
                      -- recursively eta expand  (d <pars> v)
                      up (VDef d `VApp` vlv) t'
       vs <- mapM aux ds
       app (VCon co c) (vl ++ vs) -- (map (\d -> VDef d `VApp` (vl ++ [v])) ds)
    _ -> return v
END OLD CODE -}

-- pattern matching ---------------------------------------------------

matchClauses :: Env -> [Clause] -> [Val] -> TypeCheck (Maybe Val)
matchClauses env cl vl0 = do
  vl <- mapM reduce vl0  -- REWRITE before matching (2010-07-12 dysfunctional because of lazy?)
  loop cl vl
    where loop [] vl = return Nothing
          loop (Clause _ pl Nothing : cl2) vl = loop cl2 vl -- no need to try absurd clauses
          loop (Clause _ pl (Just rhs) : cl2) vl =
              do x <- matchClause env pl rhs vl
                 case x of
                   Nothing -> loop cl2 vl
                   Just v -> return $ Just v

bindMaybe :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
bindMaybe mma k = mma >>= maybe (return Nothing) k

matchClause :: Env -> [Pattern] -> Expr -> [Val] -> TypeCheck (Maybe Val)
matchClause env pl rhs vl =
  case (pl, vl) of
    (p:pl, v:vl) -> match env p v `bindMaybe` \ env' -> matchClause env' pl rhs vl

    -- done matching: eval clause body in env and apply it to remaining arsg
    ([], _)      -> Just <$> do flip (foldM app) vl =<< whnf env rhs

    -- too few arguments to fire clause: give up
    (_, [])      -> return Nothing


match :: Env -> Pattern -> Val -> TypeCheck (Maybe Env)
match env p v0 = --trace (show env ++ show v0) $
  do
    -- force against constructor pattern or pair pattern
    v <- case p of
           ConP{}  -> do v <- force v0; traceMatch ("matching pattern " ++ show (p,v)) $ return v
           PairP{} -> do v <- force v0; traceMatch ("matching pattern " ++ show (p,v)) $ return v
           _ -> whnfClos v0
    case (p,v) of
--      (ErasedP _,_) -> return $ Just env  -- TOO BAD, DOES NOT WORK (eta!)
      (ErasedP p,_) -> match env p v
      (AbsurdP{},_) -> return $ Just env
      (DotP _,   _) -> return $ Just env
      (VarP x,   _) -> return $ Just (update env x v)
      (SizeP _ x,_) -> return $ Just (update env x v)
      (ProjP x, VProj Post y) | x == y -> return $ Just env
      (PairP p1 p2, VPair v1 v2) -> matchList env [p1,p2] [v1,v2]
      (ConP _ x [],VDef (DefId (ConK _) y)) -> failValInv v -- | x == y -> return $ Just env
--  The following case is NOT IMPOSSIBLE:
--      (ConP _ x pl,VApp (VDef (DefId (ConK _) y)) vl) -> failValInv v
      (ConP _ x pl,VApp (VDef (DefId (ConK _) y)) vl) | nameInstanceOf x  y -> matchList env pl vl
      -- If a value is a dotted record value, we do not succeed, since
      -- it is not sure this is the correct constructor.
      (ConP _ x pl,VRecord (NamedRec ri y _ dotted) rs) | nameInstanceOf x y && not (isDotted dotted) ->
         matchList env pl $ map snd rs
      (p@(ConP pi _ _), v) | coPat pi == DefPat -> do
        p <- expandDefPat p
        match env p v
      (SuccP p', v) -> (predSize <$> whnfClos v) `bindMaybe` match env p'
      (UnusableP p,_) -> throwErrorMsg ("internal error: match " ++ show (p,v))
      _ -> return Nothing

matchList :: Env -> [Pattern] -> [Val] -> TypeCheck (Maybe Env)
matchList env []     []     = return $ Just env
matchList env (p:pl) (v:vl) =
  match env p v `bindMaybe` \ env' ->
  matchList env' pl vl
matchList env pl     vl     = fail $ "matchList internal error: inequal length while trying to match patterns " ++ show pl ++ " against values " ++ show vl

-- * Typed Non-linear Matching -----------------------------------------

type GenToPattern = [(Int,Pattern)]
type MatchState = (Env, GenToPattern)

-- @nonLinMatch True@ allows also instantiation in v0
-- this is useful for finding all matching constructors
-- for an erased argument in checkPattern
nonLinMatch :: Bool -> Bool -> MatchState -> Pattern -> Val -> TVal -> TypeCheck (Maybe MatchState)
nonLinMatch undot symm st p v0 tv = traceMatch ("matching pattern " ++ show (p,v0)) $ do
  -- force against constructor pattern
  v <- case p of
         ConP{}  -> force v0
         PairP{} -> force v0
         _ -> whnfClos v0
  case (p,v) of
    (ErasedP{}, _) -> return $ Just st
    (DotP{}   , _) -> return $ Just st
    (_,    VGen i) | symm -> return $ Just $ mapSnd ((i,p):) st -- no check in case of non-lin!
    (VarP    x, _) -> matchVarP x v
    (SizeP _ x, _) -> matchVarP x v
    (ProjP x,     VProj Post y) | x == y -> return $ Just st
    (ConP _ c pl, VApp (VDef (DefId (ConK _) c')) vl) | nameInstanceOf c c' -> do
      vc <- conLType c tv
      nonLinMatchList' undot symm st pl vl vc
    -- Here, we do accept dotted constructors, since we are abusing this for unification.
    (ConP _ c pl, VRecord (NamedRec _ c' _ dotted) rs) | nameInstanceOf c c' -> do
      when undot $ clearDotted dotted
      vc <- conLType c tv
      nonLinMatchList' undot symm st pl (map snd rs) vc
    -- if the match against an unconfirmed constructor
    -- we can succeed, but not compute a sensible environment
    (_, VRecord (NamedRec _ c' _ dotted) rs) | isDotted dotted && not undot -> return $ Just st
    (p@(ConP pi _ _), v) | coPat pi == DefPat -> do
      p <- expandDefPat p
      nonLinMatch undot symm st p v tv
    (PairP p1 p2, VPair v1 v2) -> do
      tv <- force tv
      case tv of
        VQuant Sigma x dom fv -> do
          nonLinMatch undot symm st p1 v1 (typ dom) `bindMaybe` \ st -> do
          nonLinMatch undot symm st p2 v2 =<< app fv v1
        _ -> failDoc $ text "nonLinMatch: expected" <+> prettyTCM tv <+> text "to be a Sigma-type (&)"
    (SuccP p', v) -> (predSize <$> whnfClos v) `bindMaybe` \ v' ->
      nonLinMatch undot symm st p' v' tv
    _ -> return Nothing
  where
    -- Check that the previous solution for @x@ is equal to @v@.
    -- Here, we need the type!
    matchVarP x v = do
      let env = fst st
      case find ((x ==) . fst) $ envMap $ fst st of
        Nothing     -> return $ Just $ mapFst (\ env -> update env x v) st
        Just (y,v') -> ifM (eqValBool tv v v') (return $ Just st) (return Nothing)

-- nonLinMatchList symm env ps vs tv
-- typed non-linear matching of patterns ps against values vs at type tv
--   env   is the accumulator for the solution of the matching
nonLinMatchList :: Bool -> Env -> [Pattern] -> [Val] -> TVal -> TypeCheck (Maybe Env)
nonLinMatchList symm env ps vs tv =
  fmap fst <$> nonLinMatchList' False symm (env, []) ps vs tv

nonLinMatchList' :: Bool -> Bool -> MatchState -> [Pattern] -> [Val] -> TVal -> TypeCheck (Maybe MatchState)
nonLinMatchList' undot symm st [] [] tv = return $ Just st
nonLinMatchList' undot symm st (p:pl) (v:vl) tv = do
  tv <- force tv
  case tv of
    VQuant Pi x dom fv ->
      nonLinMatch undot symm st p v (typ dom) `bindMaybe` \ st' ->
      nonLinMatchList' undot symm st' pl vl =<< app fv v
    _ -> fail $ "nonLinMatchList': cannot match in absence of pi-type"
nonLinMatchList' _ _ _ _ _ _ = return Nothing


-- | Expand a top-level pattern synonym
expandDefPat :: Pattern -> TypeCheck Pattern
expandDefPat p@(ConP pi c ps) | coPat pi == DefPat = do
  PatSig ns pat v <- lookupSymbQ c
  unless (length ns == length ps) $
    fail ("underapplied defined pattern in " ++ show p)
  let pat' = if dottedPat pi then dotConstructors pat else pat
  return $ patSubst (zip ns ps) pat'
expandDefPat p = return p

---------------------------------------------------------------------------
-- * Unification
---------------------------------------------------------------------------

instance Monoid (TypeCheck Bool) where
  mempty  = return True
  mappend = andLazy
  mconcat = andM

-- | Occurrence check @nocc ks v@ (used by 'SPos' and 'TypeCheck').
--   Checks that generic values @ks@ does not occur in value @v@.
--   In the process, @tv@ is normalized.
class Nocc a where
  nocc :: [Int] -> a -> TypeCheck Bool

instance Nocc a => Nocc [a] where
  nocc = foldMap . nocc

instance Nocc a => Nocc (Dom a) where
  nocc = foldMap . nocc

instance Nocc a => Nocc (Measure a) where
  nocc = foldMap . nocc

instance Nocc a => Nocc (Bound a) where
  nocc = foldMap . nocc

instance (Nocc a, Nocc b) => Nocc (a,b) where
  nocc ks (a, b) = nocc ks a `andLazy` nocc ks b

instance Nocc a => Nocc (Sort a) where
  nocc ks (Set   v) = nocc ks v
  nocc ks (CoSet v) = nocc ks v
  nocc ks (SortC _) = mempty

instance Nocc Val where
  nocc ks v = do
    -- traceM ("nocc " ++ show v)
    v <- whnfClos v
    case v of
      -- neutrals
      VGen k                -> return $ not $ k `elem` ks
      VApp v1 vl            -> nocc ks $ v1 : vl
      VDef{}                -> mempty
      VProj{}               -> mempty
      -- Binders:
      -- ALT: do not evaluate under binders (just check environment).
      -- This is less precise but more efficient. Can give false alarms.
      -- Still sound. (Should maybe done first, like in Agda).
      VQuant pisig x dom fv -> nocc ks dom `mappend` do
                               underAbs  x dom  fv $ \ _i _xv bv -> nocc ks bv
      fv@(VLam x env b)     -> underAbs' x      fv $ \ _xv bv -> nocc ks bv
      fv@(VAbs x i u valu)  -> underAbs' x      fv $ \ _xv bv -> nocc ks bv
      fv@(VConst v)         -> underAbs' noName fv $ \ _xv bv -> nocc ks bv
      -- pairs
      VRecord _ rs          -> nocc ks $ map snd rs
      VPair v w             -> nocc ks (v, w)
      -- sizes
      VZero                 -> mempty
      VSucc v               -> nocc ks v
      VInfty                -> mempty
      VMax vl               -> nocc ks vl
      VPlus vl              -> nocc ks vl
      VSort s               -> nocc ks s
      VMeasured mu tv       -> nocc ks (mu, tv)
      VGuard beta tv        -> nocc ks (beta, tv)
      VBelow ltle v         -> nocc ks v
      VSing v tv            -> nocc ks (v, tv)
      VUp v tv              -> nocc ks (v, tv)
      VIrr                  -> mempty
      VCase v tv env cls    -> nocc ks $ v : tv : map snd (envMap env)
      -- impossible: closure (reduced away)
      VClos{}               -> fail $ "internal error: nocc " ++ show (ks,v)


-- heterogeneous typed equality and subtyping ------------------------

eqValBool :: TVal -> Val -> Val -> TypeCheck Bool
eqValBool tv v v' = errorToBool $ eqVal tv v v'
-- eqValBool tv v v' = (eqVal tv v v' >> return True) `catchError` (\ _ -> return False)

eqVal :: TVal -> Val -> Val -> TypeCheck ()
eqVal tv = leqVal' N mixed (Just (One tv))


-- force history
data Force = N | L | R -- not yet, left , right
    deriving (Eq,Show)

class Switchable a where
  switch :: a -> a

instance Switchable Force where
  switch L = R
  switch R = L
  switch N = N

instance Switchable Pol where
  switch = polNeg

instance Switchable (a,a) where
  switch (a,b) = (b,a)

instance Switchable a => Switchable (Maybe a) where
  switch = fmap switch

{-
-- WONTFIX: FOR THE FOLLOWING TO BE SOUND, ONE NEEDS COERCIVE SUBTYPING!
-- the problem is that after extraction, erased arguments are gone!
-- a function which does not use its argument can be used as just a function
-- [A] -> A <= A -> A
-- A <= [A]
leqDec :: Pol -> Dec -> Dec -> Bool
leqDec SPos  dec1 dec2 = erased dec2 || not (erased dec1)
leqDec Neg   dec1 dec2 = erased dec1 || not (erased dec2)
leqDec mixed   dec1 dec2 = erased dec1 == erased dec2
-}

-- subtyping for erasure disabled
-- but subtyping for polarities!
leqDec :: Pol -> Dec -> Dec -> Bool
leqDec p dec1 dec2 = erased dec1 == erased dec2
  && relPol p leqPol (polarity dec1) (polarity dec2)

-- subtyping ---------------------------------------------------------

subtype :: Val -> Val -> TypeCheck ()
subtype v1 v2 = -- enter ("subtype " ++ show v1 ++ "  <=  " ++ show v2) $
  leqVal' N Pos Nothing v1 v2

-- Pol ::= Pos | Neg | mixed
leqVal :: Pol -> TVal -> Val -> Val -> TypeCheck ()
leqVal p tv = leqVal' N p (Just (One tv))

type MT12 = Maybe (OneOrTwo TVal)

-- view the shape of a type or a pair of types
data TypeShape
  = ShQuant PiSigma
            (OneOrTwo Name)
            (OneOrTwo Domain)
            (OneOrTwo FVal)      -- both are function types
  | ShSort  SortShape            -- sort of same shape
  | ShData  QName (OneOrTwo TVal)-- same data, but with possibly different args
  | ShNe    (OneOrTwo TVal)      -- both neutral
  | ShSing  Val TVal             -- 1 and singleton
  | ShSingL Val TVal TVal        -- 2 and the left is a singleton
  | ShSingR TVal Val TVal        -- 2 and the right is a singleton
  | ShNone
    deriving (Eq, Ord)

data SortShape
  = ShSortC Class              -- same sort constant
  | ShSet   (OneOrTwo Val)     -- Set i and Set j
  | ShCoSet (OneOrTwo Val)     -- CoSet i and CoSet j
    deriving (Eq, Ord)

shSize = ShSort (ShSortC Size)

-- typeView does not normalize!
typeView :: TVal -> TypeShape
typeView tv =
  case tv of
    VQuant pisig x dom fv        -> ShQuant pisig (One x) (One dom) (One fv)
    VBelow{}                     -> shSize
    VSort s                      -> ShSort (sortView s)
    VSing v tv                   -> ShSing v tv
    VApp (VDef (DefId DatK n)) vs -> ShData n (One tv)
    VApp (VDef (DefId FunK n)) vs -> ShNe (One tv)  -- stuck fun
    VApp (VGen i) vs             -> ShNe (One tv)  -- type variable
    VGen i                       -> ShNe (One tv)  -- type variable
    VCase{}                      -> ShNe (One tv)  -- stuck case
    _                            -> ShNone -- error $ "typeView " ++ show tv

sortView :: Sort Val -> SortShape
sortView s =
  case s of
    SortC c -> ShSortC c
    Set   v -> ShSet   (One v)
    CoSet v -> ShCoSet (One v)

typeView12 :: (Functor m, Error e, MonadError e m) => OneOrTwo TVal -> m TypeShape
-- typeView12 :: OneOrTwo TVal -> TypeCheck TypeShape
typeView12 (One tv) = return $ typeView tv
typeView12 (Two tv1 tv2) =
  case (tv1, tv2) of
    (VQuant pisig1 x1 dom1 fv1, VQuant pisig2 x2 dom2 fv2)
      | pisig1 == pisig2 && erased (decor dom1) == erased (decor dom2) ->
        return $ ShQuant pisig1 (Two x1 x2) (Two dom1 dom2) (Two fv1 fv2)
    (VSort s1, VSort s2) -> ShSort <$> sortView12 (Two s1 s2)
    (VSing v tv, _)      -> return $ ShSingL v tv tv2
    (_, VSing v tv)      -> return $ ShSingR tv1 v tv
    _ -> case (typeView tv1, typeView tv2) of
           (ShSort s1, ShSort s2) | s1 == s2 -> return $ ShSort $ s1
           (ShData n1 _, ShData n2 _) | n1 == n2 -> return $ ShData n1 (Two tv1 tv2)
           (ShNe{}     , ShNe{}     )            -> return $ ShNe (Two tv1 tv2)
           _ -> throwError $ strMsg $ "type " ++ show tv1 ++ " has different shape than " ++ show tv2

sortView12 :: (Monad m) => OneOrTwo (Sort Val) -> m SortShape
sortView12 (One s) = return $ sortView s
sortView12 (Two s1 s2) =
  case (s1, s2) of
    (SortC c1, SortC c2) | c1 == c2 -> return $ ShSortC c1
    (Set v1, Set v2)                -> return $ ShSet (Two v1 v2)
    (CoSet v1, CoSet v2)            -> return $ ShCoSet (Two v1 v2)
    _ -> fail $ "sort " ++ show s1 ++ " has different shape than " ++ show s2

whnf12 :: OneOrTwo Env -> OneOrTwo Expr -> TypeCheck (OneOrTwo Val)
whnf12 env12 e12 = Traversable.traverse id $ zipWith12 whnf env12 e12

app12 ::  OneOrTwo Val -> OneOrTwo Val -> TypeCheck (OneOrTwo Val)
app12 fv12 v12 = Traversable.traverse id $ zipWith12 app fv12 v12

-- if m12 = Nothing, we are checking subtyping, otherwise we are
-- comparing objects or higher-kinded types
-- if two types are given (heterogeneous equality), they need to be
-- of the same shape, otherwise they cannot contain common terms
leqVal' :: Force -> Pol -> MT12 -> Val -> Val -> TypeCheck ()
leqVal' f p mt12 u1' u2' = local (\ cxt -> cxt { consistencyCheck = False }) $ do
 -- 2013-03-30 During subtyping, it is fine to add any size hypotheses.
 l <- getLen
 ren <- getRen
 enterDoc (case mt12 of
  Nothing -> -- text ("leqVal' (subtyping) " ++ show  (Map.toList $ ren) ++ " |-")
             text "leqVal' (subtyping) "
             <+> prettyTCM u1' <+> text (" <=" ++ show p ++ " ")
             <+> prettyTCM u2'
  Just (One tv) -> -- text ("leqVal' " ++ show  (Map.toList $ ren) ++ " |-")
             text "leqVal' "
             <+> prettyTCM u1' <+> text (" <=" ++ show p ++ " ")
             <+> prettyTCM u2' <+> colon
             <+> prettyTCM tv
  Just (Two tv1 tv2) -> -- text ("leqVal' " ++ show  (Map.toList $ ren) ++ " |-")
             text "leqVal' "
             <+> prettyTCM u1' <+> colon
             <+> prettyTCM tv1 <+> text (" <=" ++ show p ++ " ")
             <+> prettyTCM u2' <+> colon
             <+> prettyTCM tv2) $ do
{-
    ce <- ask
    trace  (("rewrites: " +?+ show (rewrites ce)) ++ "  leqVal': " ++ show ce ++ "\n |- " ++ show u1' ++ "\n  <=" ++ show p ++ "  " ++ show u2') $
-}
    mt12f <- mapM (mapM force) mt12 -- leads to LOOP, see HungryEta.ma
    sh12 <- case mt12f of
              Nothing -> return Nothing
              Just tv12 -> case typeView12 tv12 of
                Right sh -> return $ Just sh
                Left err -> (recoverFail err) >> return Nothing
    case sh12 of

      -- subtyping directed by common type shape

      Just (ShSing{}) -> return () -- two terms are equal at singleton type!
      Just (ShSingL v1 tv1' tv2) -> leqVal' f p (Just (Two tv1' tv2)) v1 u2'
      Just (ShSingR tv1 v2 tv2') -> leqVal' f p (Just (Two tv1 tv2')) u1' v2
      Just (ShSort (ShSortC Size)) -> leqSize p u1' u2'

{-  functions are compared pointwise

   Gamma, p(x:A) |- t x : B  <=  Gamma', p'(x:A') |- t' x : B'
   ----------------------------------------------------------
   Gamma |- t : p(x:A) -> B  <=  Gamma' |- t' : p'(x:A') -> B'
-}
      Just (ShQuant Pi x12 dom12 fv12) -> do
         x <- do
           let x = name12 x12
           if null (suggestion x) then do
             case (u1', u2') of
               (VLam x _ _, _) -> return x
               (_, VLam x _ _) -> return x
               _ -> return x
            else return x
         newVar x dom12 $ \ _ xv12 -> do
            u1' <- app u1' (first12  xv12)
            u2' <- app u2' (second12 xv12)
            tv12 <- app12 fv12 xv12
            leqVal' f p (Just tv12) u1' u2'
{-
      Just (VPi x1 dom1 env1 b1, VPi x2 dom2 env2 b2)  ->
         new2 x1 (dom1, dom2) $ \ (xv1, xv2) -> do
            u1' <- app u1' xv1
            u2' <- app u2' xv2
            tv1' <- whnf (update env1 x1 xv1) b1
            tv2' <- whnf (update env2 x2 xv2) b2
            leqVal' f p (Just (tv1', tv2')) u1' u2'
-}


      -- structural subtyping (not directed by types)

      _ -> do
       u1 <- reduce =<< whnfClos u1'
       u2 <- reduce =<< whnfClos u2'

       let tryForcing fallback = do
            (f1,u1f) <- force' False u1
            (f2,u2f) <- force' False u2
            case (f1,f2) of -- (u1f /= u1,u2f /= u2) of

              (True,False) | f /= R -> -- only unroll one side
                 enter ("forcing LHS") $
                           leqVal' L p mt12 u1f u2
              (False,True) | f /= L ->
                 enter ("forcing RHS") $
                           leqVal' R p mt12 u1 u2f
              _ -> -- enter ("not forcing " ++ show (f1,f2,f)) $
                     fallback

           leqCons n1 vl1 n2 vl2 = do
                 unless (n1 == n2) $
                  recoverFail $
                    "leqVal': head mismatch "  ++ show u1 ++ " != " ++ show u2
                 case mt12 of
                   Nothing -> recoverFail $ "leqVal': cannot compare constructor terms without type"
                   Just tv12 -> do
                     ct12 <- Traversable.mapM (conType n1) tv12
                     leqVals' f p ct12 vl1 vl2
                     return ()
{-
       leqStructural u1 u2 where
          leqStructural u1 u2 =
-}
       case (u1,u2) of

{-
  C = C'  (proper: C' entails C, but I do not want to implement entailment)
  Gamma, C |- A  <=  Gamma', C' |- A'
  -----------------------------------------
  Gamma |- C ==> A  <=  Gamma' |- C' ==> A'
-}
              (VGuard beta1 bv1, VGuard beta2 bv2) -> do
                 entailsGuard (switch p) beta1 beta2
                 leqVal' f p Nothing bv1 bv2

              (VGuard beta u1, u2) | p `elem` [Neg,Pos] ->
                addOrCheckGuard (switch p) beta $
                  leqVal' f p Nothing u1 u2

              (u1, VGuard beta u2) | p `elem` [Neg,Pos] ->
                addOrCheckGuard p beta $
                  leqVal' f p Nothing u1 u2
 {-
  p' <= p
  Gamma' |- A' <= Gamma |- A
  Gamma, p(x:A) |- B <= Gamma', p'(x:A') |- B'
  ---------------------------------------------------------
  Gamma |- p(x:A) -> B : s <= Gamma' |- p'(x:A') -> B' : s'
-}
              (VQuant piSig1 x1 dom1@(Domain av1 _ dec1) fv1,
               VQuant piSig2 x2 dom2@(Domain av2 _ dec2) fv2) -> do
                 let p' = if piSig1 == Pi then switch p else p
                 if piSig1 /= piSig2 || not (leqDec p' dec1 dec2) then
                    recoverFailDoc $ text "subtyping" <+> prettyTCM u1 <+> text (" <=" ++ show p ++ " ") <+> prettyTCM u2 <+> text "failed"
                  else do
                    leqVal' (switch f) p' Nothing av1 av2
                    -- take smaller domain
                    let dom = if (p' == Neg) then dom2 else dom1
                    let x = bestName $ if p' == Neg then [x2,x1] else [x1,x2]
                    new x dom $ \ xv -> do
                      bv1 <- app fv1 xv
                      bv2 <- app fv2 xv
                      enterDoc (text "comparing codomain" <+> prettyTCM bv1 <+> text "with" <+> prettyTCM bv2) $
                        leqVal' f p Nothing bv1 bv2

              (VSing v1 av1, VSing v2 av2) -> do
                  leqVal' f p Nothing av1 av2
                  leqVal' N mixed (Just (Two av1 av2)) v1 v2  -- compare for eq.

              (VSing v1 av1, VBelow ltle v2) | av1 == vSize && p == Pos -> do
                 v1 <- whnfClos v1
                 leSize ltle p v1 v2

{- 2012-01-28 now vSize is VBelow Le Infty

              -- extra cases since vSize is not implemented as VBelow Le Infty
              (u1,u2) | isVSize u1 && isVSize u2 -> return ()
              (VSort (SortC Size), VBelow{}) -> leqStructural (VBelow Le VInfty) u2
              (VBelow{}, VSort (SortC Size)) -> leqStructural u1 (VBelow Le VInfty)
-}
              -- care needed to not make <=# a subtype of <#
              (VBelow ltle1 v1, VBelow ltle2 v2) ->
                case (p, ltle1, ltle2) of
                  _ | ltle1 == ltle2 -> leSize Le p v1 v2
                  (Neg, Le, Lt) -> leSize Le p (vSucc v1) v2
                  (Neg, Lt, Le) -> leSize Lt p v1 v2  -- careful here
                  (p  , Lt, Le) -> leSize Le p v1 (vSucc v2)
                  (p  , Le, Lt) -> leSize Lt p v1 v2  -- careful here

              -- unresolved eta-expansions (e.g. at coinductive type)
              (VUp v1 av1, VUp v2 av2) -> do
                  -- leqVal' f p Nothing av1 av2      -- do not compare types
                  leqVal' f p (Just (Two av1 av2)) v1 v2  -- OR: Just(tv1,tv2) ?
              (VUp v1 av1, u2) -> leqVal' f p mt12 v1 u2
              (u1, VUp v2 av2) -> leqVal' f p mt12 u1 v2

              (VRecord (NamedRec _ n1 _ _) rs1, VRecord (NamedRec _ n2 _ _) rs2) ->
                 leqCons n1 (map snd rs1) n2 (map snd rs2)

{-
              -- the following three cases should be impossible
              -- but aren't.  I gave up on this bug -- 2012-01-25
              -- FOUND IT

              (VRecord (NamedRec _ n1 _) rs1,
               VApp v2@(VDef (DefId (ConK _) n2)) vl2) -> leqCons n1 (map snd rs1) n2 vl2

              (VApp v1@(VDef (DefId (ConK _) n1)) vl1,
               VRecord (NamedRec _ n2 _) rs2) -> leqCons n1 vl1 n2 (map snd rs2)

              (VApp v1@(VDef (DefId (ConK _) n1)) vl1,
               VApp v2@(VDef (DefId (ConK _) n2)) vl2) -> leqCons n1 vl1 n2 vl2
-}

              -- smart equality is not transitive
              (VCase v1 tv1 env1 cl1, VCase v2 tv2 env2 cl2) -> do
                 leqVal' f p (Just (Two tv1 tv2)) v1 v2 -- FIXED: do not have type here, but v1,v2 are neutral
                 leqClauses f p mt12 v1 tv1 env1 cl1 env2 cl2

{- REMOVED, NOT TRANSITIVE
              (VCase v env cl, v2) -> leqCases (switch f) (switch p) (switch mt12) v2 v env cl
              (v1, VCase v env cl) -> leqCases f p mt12 v1 v env cl
-}
              (VSing v1 av1, av2)  -> leqVal' f p Nothing av1 av2  -- subtyping ax
              (VSort s1, VSort s2) -> leqSort p s1 s2
              (a1,a2) | a1 == a2 -> return ()
              (u1,u2) -> tryForcing $
                case (u1,u2) of
                  (VApp v1 vl1, VApp v2 vl2) -> leqApp f p v1 vl1 v2 vl2
                  (VApp v1 vl1, u2) -> leqApp f p v1 vl1 u2 []
                  (u1, VApp v2 vl2) -> leqApp f p u1 []  v2 vl2
                  _ -> leqApp f p u1 [] u2 []

leqClauses :: Force -> Pol -> MT12 -> Val -> TVal -> Env -> [Clause] -> Env -> [Clause] -> TypeCheck ()
leqClauses f pol mt12 v tvp env1 cls1 env2 cls2 = loop cls1 cls2 where
  loop cls1 cls2 = case (cls1,cls2) of
    ([],[]) -> return ()
    (Clause _ [p1] mrhs1 : cls1', Clause _ [p2] mrhs2 : cls2') -> do
      ns <- flip execStateT [] $ alphaPattern p1 p2
      case (mrhs1, mrhs2) of
        (Nothing, Nothing) -> return ()
        (Just e1, Just e2) -> do
            let tv = maybe vTopSort first12 mt12
            let tv012 = maybe [] toList12 mt12
            addPattern (tvp `arrow` tv) p2 env2 $ \ _ pv env2' ->
              addRewrite (Rewrite v pv) tv012 $ \ tv012 -> do
                let env1' = env2' { envMap = compAssoc ns (envMap env2') }
                v1  <- whnf (appendEnv env1' env1) e1
                v2  <- whnf (appendEnv env2' env2) e2
                leqVal' f pol (toMaybe12 tv012) v1 v2
            loop cls1' cls2'
{-
-- naive implementation for now
leqClauses :: Force -> Pol -> MT12 -> Val -> TVal -> Env -> [Clause] -> Env -> [Clause] -> TypeCheck ()
leqClauses f pol mt12 v tvp env1 cls1 env2 cls2 = loop cls1 cls2 where
  loop cls1 cls2 = case (cls1,cls2) of
    ([],[]) -> return ()
    (Clause _ [p1] mrhs1 : cls1', Clause _ [p2] mrhs2 : cls2') -> do
      eqPattern p1 p2
      case (mrhs1, mrhs2) of
        (Nothing, Nothing) -> return ()
        (Just e1, Just e2) -> do
            let tv = maybe vTopSort first12 mt12
            let tv012 = maybe [] toList12 mt12
            addPattern (tvp `arrow` tv) p1 env1 $ \ _ pv env' ->
              addRewrite (Rewrite v pv) tv012 $ \ tv012 -> do
                v1  <- whnf (appendEnv env' env1) e1
                v2  <- whnf (appendEnv env' env2) e2
                leqVal' f pol (toMaybe12 tv012) v1 v2
            loop cls1' cls2'

eqPattern :: Pattern -> Pattern -> TypeCheck ()
eqPattern p1 p2 = if p1 == p2 then return () else fail $ "pattern " ++ show p1 ++ " != " ++ show p2
-}

type NameMap = [(Name,Name)]

alphaPattern :: Pattern -> Pattern -> StateT NameMap TypeCheck ()
alphaPattern p1 p2 = do
  let failure = fail $ "pattern " ++ show p1 ++ " != " ++ show p2
      alpha x1 x2 = do
        ns <- get
        case lookup x1 ns of
          Nothing -> put $ (x1,x2) : ns
          Just x2' | x2 == x2' -> return ()
                   | otherwise -> failure
  case (p1,p2) of
    (VarP x1, VarP x2) -> alpha x1 x2
    (ConP pi1 n1 ps1, ConP pi2 n2 ps2) | pi1 == pi2 && n1 == n2 ->
      zipWithM_ alphaPattern ps1 ps2
    (SuccP p1, SuccP p2) -> alphaPattern p1 p2
    (SizeP _ x1, SizeP _ x2) -> alpha x1 x2
    (PairP p11 p12, PairP p21 p22) -> do
      alphaPattern p11 p21
      alphaPattern p12 p22
    (ProjP n1, ProjP n2) -> unless (n1 == n2) failure
    (DotP _, DotP _) -> return ()
    (AbsurdP, AbsurdP) -> return ()
    (ErasedP p1, ErasedP p2) -> alphaPattern p1 p2
    (UnusableP p1, UnusableP p2) -> alphaPattern p1 p2
    _ -> failure

-- leqCases f p tv1 v1 v tv env cl
-- checks whether  v1 <=p (VCase v tv env cl) : tv1
leqCases :: Force -> Pol -> MT12 -> Val -> Val -> TVal -> Env -> [Clause] -> TypeCheck ()
leqCases f pol mt12 v1 v tvp env cl = do
  vcase <- evalCase v tvp env cl
  case vcase of
    (VCase v tvp env cl) -> mapM_ (leqCase f pol mt12 v1 v tvp env) cl
    v2 -> leqVal' f pol mt12 v1 v2

-- absurd cases need not be checked
leqCase :: Force -> Pol -> MT12 -> Val -> Val -> TVal -> Env -> Clause -> TypeCheck ()
leqCase f pol mt12 v1 v tvp env (Clause _ [p] Nothing) = return ()
leqCase f pol mt12 v1 v tvp env (Clause _ [p] (Just e)) = enterDoc (text "leqCase" <+> prettyTCM v <+> text " --> " <+> text (show p ++ "  |- ") <+> prettyTCM v1 <+> text (" <=" ++ show pol ++ " ") <+> prettyTCM (VClos env e)) $ do    -- ++ "  :  " ++ show mt12) $
-- the dot patterns inside p are only valid in environment env
  let tv = case mt12 of
             Nothing -> vTopSort
             Just tv12 -> second12 tv12
  addPattern (tvp `arrow` tv) p env $ \ _ pv env' ->
    addRewrite (Rewrite v pv) [tv,v1] $ \ [tv',v1'] -> do
      v2  <- whnf (appendEnv env' env) e
      v2' <- reval v2 -- 2010-09-10, WHY?
      let mt12' = fmap (mapSecond12 (const tv')) mt12
      leqVal' f pol mt12' v1' v2'

-- compare spines (see rule Al-App-Ne, Abel, MSCS 08)
-- q ::= mixed | Pos | Neg
leqVals' :: Force -> Pol -> OneOrTwo TVal -> [Val] -> [Val] -> TypeCheck (OneOrTwo TVal)
leqVals' f q tv12 vl1 vl2 = do
  sh12 <- typeView12 =<< mapM force tv12
  case (vl1, vl2, sh12) of

    ([], [], _) -> return tv12

    (VProj Post p1 : vs1, VProj Post p2 : vs2, ShData d _) -> do
      unless (p1 == p2) $
        recoverFailDoc $ text "projections"
          <+> prettyTCM p1 <+> text "and"
          <+> prettyTCM p2 <+> text "differ!"
        -- recoverFail $ "projections " ++ show p1 ++ " and " ++ show p2 ++ " differ!"
      tv12 <- mapM (\ tv -> projectType tv p1 VIrr) tv12
      leqVals' f q tv12 vs1 vs2

    (w1:vs1, w2:vs2, ShQuant Pi x12 dom12 fv12) -> do
      let p = oneOrTwo id polAnd (fmap (polarity . decor) dom12)
      let dec = Dec p -- WAS: , erased = erased $ decor $ first12 dom12 }
      v1 <- whnfClos w1
      v2 <- whnfClos w2
      tv12 <- do
        if erased p -- WAS: (erased dec || p == Pol.Const)
         -- we have skipped an argument, so proceed with two types!
         then app12 (toTwo fv12) (Two v1 v2)
         else do
           let q' = polComp p q
           applyDec dec $
             leqVal' f q' (Just $ fmap typ dom12) v1 v2
           -- we have not skipped comparison, so proceed (1/2) as we came in
           case fv12 of
             Two{}  -> app12 fv12 (Two v1 v2)
             One fv -> One <$> app fv v1
               -- type is invariant, so it does not matter which one we take
      leqVals' f q tv12 vs1 vs2

    _ -> failDoc $ text "leqVals': not (compatible) function types or mismatch number of arguments when comparing "
           <+> prettyTCM vl1 <+> text " to "
           <+> prettyTCM vl2 <+> text " at type "
           <+> prettyTCM tv12
--    _ -> fail $ "leqVals': not (compatible) function types or mismatch number of arguments when comparing  " ++ show vl1 ++ "  to  " ++ show vl2 ++ "  at type  " ++ show tv12

{-
leqVals' f q (VPi x1 dom1@(Domain av1 _ dec1) env1 b1,
              VPi x2 dom2@(Domain av2 _ dec2) env2 b2)
         (w1:vs1) (w2:vs2) | dec1 == dec2 = do
  let p = polarity dec1
  v1 <- whnfClos w1
  v2 <- whnfClos w2
  when (not (erased dec1)) $
    applyDec dec1 $ leqVal' f (polComp p q) (Just (av1,av2)) v1 v2
  tv1 <- whnf (update env1 x1 v1) b1
  tv2 <- whnf (update env2 x2 v2) b2
  leqVals' f q (tv1,tv2) vs1 vs2
-}

{-
leqNe :: Force -> Val -> Val -> TypeCheck TVal
leqNe f v1 v2 = --trace ("leqNe " ++ show v1 ++ "<=" ++ show v2) $
  do case (v1,v2) of
      (VGen k1, VGen k2) -> if k1 == k2 then do
                                 dom <- lookupGem k1
                                 return $ typ dom
                               else throwErrorMsg $ "gen mismatch "  ++ show k1 ++ " " ++ show k2
-}

-- leqApp f pol v1 vs1 v2 vs2    checks   v1 vs1 <=pol v2 vs2
-- pol ::= Param | Pos | Neg
leqApp :: Force -> Pol -> Val -> [Val] -> Val -> [Val] -> TypeCheck ()
leqApp f pol v1 w1 v2 w2 = {- trace ("leqApp: " -- ++ show delta ++ " |- "
                                  ++ show v1 ++ show w1 ++ " <=" ++ show pol ++ " " ++ show v2 ++ show w2) $ -}
{-
  do let headMismatch = recoverFail $
            "leqApp: head mismatch "  ++ show v1 ++ " != " ++ show v2
-}
  do let headMismatch = recoverFailDoc $ text "leqApp: head mismatch"
           <+> prettyTCM v1 <+> text "!=" <+> prettyTCM v2
     let emptyOrUnit u1 u2 =
          unlessM (isEmptyType u1) $ unlessM (isUnitType u2) $ headMismatch
     case (v1,v2) of
{-  IMPOSSIBLE:
      (VApp v1 [], v2) -> leqApp f pol v1 w1 v2 w2
      (v1, VApp v2 []) -> leqApp f pol v1 w1 v2 w2
-}
{-
      (VApp{}, _)    -> throwErrorMsg $ "leqApp: internal error: hit application v1 = " ++ show v1
      (_, VApp{})    -> throwErrorMsg $ "leqApp: internal error: hit application v2 = " ++ show v2
-}

      (VUp v1 _, v2) -> leqApp f pol v1 w1 v2 w2
      (v1, VUp v2 _) -> leqApp f pol v1 w1 v2 w2

      (VGen k1, VGen k2) | k1 == k2 -> do
        tv12 <- (fmap typ . domain) <$> lookupGen k1
        leqVals' f pol tv12 w1 w2
        return ()
{-
      (VGen k1, VGen k2) ->
        if k1 /= k2
          then headMismatch
          else do tv12 <- (fmap typ . domain) <$> lookupGen k1
                  leqVals' f pol tv12 w1 w2
                  return ()
-}
{-
      (VCon _ n, VCon _ m) ->
        if n /= m
         then throwErrorMsg $
            "leqApp: head mismatch "  ++ show v1 ++ " != " ++ show v2
         else do
          sige <- lookupSymb n
          case sige of
            (ConSig tv) -> -- constructor
               leqVals' f tv (repeat mixed) w1 w2 >> return ()
-}

      (VDef n, VDef m) | n == m ->  do
        tv <- lookupSymbTypQ (idName n)
        leqVals' f pol (One tv) w1 w2
        return ()

      -- check for least or greatest type

      (u1,u2) -> if pol == Pos then emptyOrUnit u1 u2 else
                 if pol == Neg then emptyOrUnit u2 u1 else headMismatch

{-
      -- least type
      (VDef (DefId DatK n), v2) | pol == Pos ->
        ifM (isEmptyData n) (return ()) headMismatch
      (v1, VDef (DefId DatK n)) | pol == Neg ->
        ifM (isEmptyData n) (return ()) headMismatch
-}
{-
      (VDef n, VDef m) ->
        if (name n) /= (name m) then do
           bot <- if pol==Neg then isEmptyData $ name m else
                  if pol==Pos then isEmptyData $ name n else return False
           if bot then return () else headMismatch
         else do
           tv <- lookupSymbTyp (name n)
           leqVals' f pol (One tv) w1 w2
           return ()
-}
{-
          sig <- gets signature
          case lookupSig (name n) sig of
            (DataSig{ numPars = p, positivity = pos, isSized = s, isCo = co, symbTyp = tv }) -> -- data type
               let positivitySizeIndex = if s /= Sized then mixed else
                                           if co == Ind then Pos else Neg
                   pos' = -- trace ("leqApp:  posOrig = " ++ show (pos ++ [positivitySizeIndex])) $
                     map (polComp pol) (pos ++ positivitySizeIndex : repeat mixed) -- the polComp will replace all SPos by Pos
               in leqVals' f tv pos' w1 w2
                    >> return ()

-- otherwise, we are dealing with a (co) recursive function or a constructor
            entry -> leqVals' f (symbTyp entry) (repeat mixed) w1 w2 >> return ()
-}

{-
      _ -> headMismatch

      _ -> recoverFail $ "leqApp: " ++ show v1 ++ show w1 ++ " !<=" ++ show pol ++ " " ++ show v2 ++ show w2
-}

isEmptyType :: TVal -> TypeCheck Bool
isEmptyType (VDef (DefId DatK n)) = isEmptyData n
isEmptyType _ = return False

isUnitType :: TVal -> TypeCheck Bool
isUnitType (VDef (DefId DatK n)) = isUnitData n
isUnitType _ = return False

-- comparing sorts and sizes -----------------------------------------

leqSort :: Pol -> Sort Val -> Sort Val -> TypeCheck ()
leqSort p = relPolM p leqSort'
{-
leqSort mixed s1 s2 = leqSort' s1 s2 >> leqSort' s2 s1
leqSort Neg s1 s2 = leqSort' s2 s1
leqSort Pos s1 s2 = leqSort' s1 s2
-}

leqSort' :: Sort Val -> Sort Val -> TypeCheck ()
leqSort' s1 s2 = do
--  let err = "universe test " ++ show s1 ++ " <= " ++ show s2 ++ " failed"
  let err = text "universe test"
            <+> prettyTCM s1 <+> text "<="
            <+> prettyTCM s2 <+> text "failed"
  case (s1,s2) of
     (_            , Set VInfty)         -> return ()
     (SortC c      , SortC c') | c == c' -> return ()
     (Set v1       , Set v2)             -> leqSize Pos v1 v2
     (CoSet VInfty , Set v)              -> return ()
     (Set VZero    , CoSet{})            -> return ()
     (CoSet v1     , CoSet v2)           -> leqSize Neg v1 v2
     _ -> recoverFailDoc err

minSize :: Val -> Val -> Maybe Val
minSize v1 v2 =
  case (v1,v2) of
    (VZero,_)  -> return VZero
    (_,VZero)  -> return VZero
    (VInfty,_) -> return v2
    (_,VInfty) -> return v1
    (VMax vs,_) -> maxMins $ map (\ v -> minSize v v2) vs
    (_,VMax vs) -> maxMins $ map (\ v -> minSize v1 v) vs
    (VSucc v1', VSucc v2') -> fmap succSize $ minSize v1' v2'
    (VGen i, VGen j) -> if i == j then return $ VGen i else Nothing
    (VSucc v1', VGen j) -> minSize v1' v2
    (VGen i, VSucc v2') -> minSize v1 v2'

maxMins :: [Maybe Val] -> Maybe Val
maxMins mvs = case compressMaybes mvs of
                     [] -> Nothing
                     vs' -> return $ maxSize vs'

-- substaging on size values
leqSize :: Pol -> Val -> Val -> TypeCheck ()
leqSize = leSize Le

ltSize :: Val -> Val -> TypeCheck ()
ltSize = leSize Lt Pos

leSize :: LtLe -> Pol -> Val -> Val -> TypeCheck ()
leSize ltle pol v1 v2 = enterDoc (text "leSize"
      <+> prettyTCM v1 <+> text (show ltle ++ show pol)
      <+> prettyTCM v2) $
-- enter ("leSize " ++ show v1 ++ " " ++ show ltle ++ show pol ++ " " ++ show v2) $
    traceSize ("leSize " ++ show v1 ++ " " ++ show ltle ++ show pol ++ " " ++ show v2) $
    do case (v1,v2) of
         _ | v1 == v2 && ltle == Le -> return () -- TODO: better handling of sums!
         (VSucc v1,VSucc v2) -> leSize ltle pol v1 v2
{-
         (VGen i1,VGen i2) -> do
           d <- getSizeDiff i1 i2 -- check size relation from constraints
           case d of
             Nothing -> recoverFail $ "leqSize: head mismatch: " ++ show v1 ++ " !<= " ++ show v2
             Just k -> case (pol,k) of
               (_, 0) | pol == mixed -> return ()
               (Pos, _) | k >= 0 -> return ()
               (Neg, _) | k <= 0 -> return ()
               _ ->  recoverFail $ "leqSize: " ++ show v1 ++ " !<=" ++ show pol ++ " " ++ show v2 ++ " failed"
-}
{-
           if v1 == v2 then return ()
           else throwErrorMsg $ "leqSize: head mismatch: " ++ show v1 ++ " !<= " ++ show v2
-}
         (VInfty,VInfty) | ltle == Le -> return ()
                         | otherwise -> recoverFail "leSize: # < # failed"
         (VApp h1 tl1,VApp h2 tl2) -> leqApp N pol h1 tl1 h2 tl2
         _ -> relPolM pol (leSize' ltle) v1 v2

leqSize' :: Val -> Val -> TypeCheck ()
leqSize' = leSize' Le

leSize' :: LtLe -> Val -> Val -> TypeCheck ()
leSize' ltle v1 v2 = -- enter ("leSize' " ++ show v1 ++ " " ++ show ltle ++ " " ++ show v2) $
  enterDoc (text "leSize'" <+> prettyTCM v1 <+> text (show ltle) <+> prettyTCM v2) $
    traceSize ("leSize' " ++ show v1 ++ " " ++ show ltle ++ " " ++ show v2) $
    do let failure = recoverFailDoc $ text "leSize':"
             <+> prettyTCM v1 <+> text (show ltle)
             <+> prettyTCM v2 <+> text "failed"
           -- err = "leSize': " ++ show v1 ++ " " ++ show ltle ++ " " ++ show v2 ++ " failed"
       case (v1,v2) of
         (VZero,_) | ltle == Le -> return ()
         (VSucc{}, VZero) -> failure
         (VInfty, VZero) -> failure
         (VGen{}, VZero) -> failure
         (VMax vs,_) -> mapM_ (\ v -> leSize' ltle v v2) vs -- all v in vs <= v2
         (_,VMax vs)  -> foldr1 orM $ map (leSize' ltle v1) vs -- this produces a disjunction
--         (_,VMax _)  -> addLe ltle v1 v2 -- this produces a disjunction
         (_,VInfty) | ltle == Le -> return ()
         (VZero, VInfty) -> return ()
         (VMeta{},VZero) -> addLe ltle v1 v2
{-
         (0,VMeta i n', VMeta j m') ->
           let (n,m) = if bal <= 0 then (n', m' - bal) else (n' + bal, m') in
-}
         (VMeta i rho n, VMeta j rho' m) ->
               addLe ltle (VMeta i rho  (n - min n m))
                        (VMeta j rho' (m - min n m))
         (VMeta i rho n, VSucc v2) | n > 0 -> leSize' ltle (VMeta i rho (n-1)) v2
         (VMeta i rho n, v2)  -> addLe ltle v1 v2
         (VSucc v1, VMeta i rho n) | n > 0 -> leSize' ltle v1 (VMeta i rho (n-1))
         (v1,VMeta i rho n) -> addLe ltle v1 v2
         _ -> leSize'' ltle 0 v1 v2
{- HANDLED BY leSize'' ltle
         (VSucc{}, VGen{}) -> fail err
         (VSucc{}, VPlus{}) -> fail err
-}
-- leSize'' ltle bal v v'  checks whether  Succ^bal v `lt` v'
-- invariant: bal is zero in cases for VMax and VMeta
leSize'' :: LtLe -> Int -> Val -> Val -> TypeCheck ()
leSize'' ltle bal v1 v2 = traceSize ("leSize'' " ++ show v1 ++ " + " ++ show bal ++ " " ++ show ltle ++ " " ++ show v2) $
    do let failure = recoverFailDoc (text "leSize'':" <+> prettyTCM v1 <+> text ("+ " ++ show bal) <+> text (show ltle) <+> prettyTCM v2 <+> text "failed")
           check mb = ifM mb (return ()) failure
           ltlez = case ltle of { Le -> 0 ; Lt -> -1 }
       case (v1,v2) of
#ifdef STRICTINFTY
-- Only cancel variables < #
         _ | v1 == v2 && ltle == Le && bal <= 0 -> return ()
         (VGen i, VGen j) | i == j && bal <= -1 -> check $ isBelowInfty i
#else
-- Allow cancelling of all variables
         _ | v1 == v2 && bal <= ltlez -> return () -- TODO: better handling of sums!
#endif
         (VGen i, VInfty) | ltle == Lt -> check $ isBelowInfty i
         (VZero,_) | bal <= ltlez -> return ()
         (VZero,VInfty) -> return ()
         (VZero,VGen _) | bal > ltlez -> recoverFailDoc $ text "0 not <" <+> prettyTCM v2
         (VSucc v1, v2) -> leSize'' ltle (bal + 1) v1 v2
         (v1, VSucc v2) -> leSize'' ltle (bal - 1) v1 v2
         (VPlus vs1, VPlus vs2) -> leSizePlus ltle bal vs1 vs2
         (VPlus vs1, VZero) -> leSizePlus ltle bal vs1 []
         (VZero, VPlus vs2) -> leSizePlus ltle bal [] vs2
         (VPlus vs1, _) -> leSizePlus ltle bal vs1 [v2]
         (_, VPlus vs2) -> leSizePlus ltle bal [v1] vs2
         (VZero,_) -> leSizePlus ltle bal [] [v2]
         (_,VZero) -> leSizePlus ltle bal [v1] []
         _ -> leSizePlus ltle bal [v1] [v2]

#if (defined STRICTINFTY)
{-  2012-02-06 this modification cancels only variables < #
    However, omega-instantiation is valid [i < #] -> F i subseteq F #
    because every chain has a limit at #.
-}
leSizePlus :: LtLe -> Int -> [Val] -> [Val] -> TypeCheck ()
leSizePlus Lt bal vs1 vs2 = do
  vs2' <- filterM varBelowInfty vs2
  vs1' <- filterM varBelowInfty vs1
  leSizePlus' Lt bal (vs1 List.\\ vs2') (vs2 List.\\ vs1')
leSizePlus Le bal vs1 vs2 =
  leSizePlus' Le bal (vs1 List.\\ vs2) (vs2 List.\\ vs1)
#else
leSizePlus :: LtLe -> Int -> [Val] -> [Val] -> TypeCheck ()
leSizePlus ltle bal vs1 vs2 =
  leSizePlus' ltle bal (vs1 List.\\ vs2) (vs2 List.\\ vs1)
#endif


varBelowInfty :: Val -> TypeCheck Bool
varBelowInfty (VGen i) = isBelowInfty i
varBelowInfty _        = return False

leSizePlus' :: LtLe -> Int -> [Val] -> [Val] -> TypeCheck ()
leSizePlus' ltle bal vs1 vs2 = do
  let v1 = plusSizes vs1
  let v2 = plusSizes vs2
  let exit True  = return ()
      exit False | bal >= 0  = recoverFailDoc (text "leSize:" <+> prettyTCM v1 <+> text ("+ " ++ show bal ++ " " ++ show ltle) <+> prettyTCM v2 <+> text "failed")
                 | otherwise = recoverFailDoc (text "leSize:" <+> prettyTCM v1 <+> text (show ltle) <+> prettyTCM v2 <+> text ("+ " ++ show (-bal) ++ " failed"))
  traceSizeM ("leSizePlus' ltle " ++ show v1 ++ " + " ++ show bal ++ " " ++ show ltle ++ " " ++ show v2)
  let ltlez = case ltle of { Le -> 0 ; Lt -> -1 }
  case (vs1,vs2) of
    ([],_) | bal <= ltlez -> return ()
    ([],[VGen i]) -> do
      n <- getMinSize i
      -- traceM ("getMinSize = " ++ show n)
      case n of
        Nothing -> exit False -- height of VGen i == 0
        Just n  -> exit (bal <= n + ltlez)
    ([VGen i1],[VGen i2]) -> do
      d <- sizeVarBelow i1 i2
      traceSizeM ("sizeVarBelow " ++ show (i1,i2) ++ " returns " ++ show d)
      case d of
        Nothing -> tryIrregularBound i1 i2 (ltlez - bal)
-- recoverFail $ "leSize: head mismatch: " ++ show v1 ++ " " ++ show ltle ++ " " ++ show v2
        Just k -> exit (bal <= k + ltlez)
    _ -> exit False

-- BAD HACK!
-- check (VGen i1) <= (VGen i2) + k
tryIrregularBound :: Int -> Int -> Int -> TypeCheck ()
tryIrregularBound i1 i2 k = do
  betas <- asks bounds
  let beta = Bound Le (Measure [VGen i1]) (Measure [iterate VSucc (VGen i2) !! k])
  foldl (\ result beta' -> result `orM` entailsGuard Pos beta' beta)
    (recoverFail "bound not entailed")
    betas

{-
leqSize' :: Val -> Val -> TypeCheck ()
leqSize' v1 v2 = --trace ("leqSize' " ++ show v1 ++ show v2) $
    do case (v1,v2) of
         (VMax vs,_) -> mapM_ (\ v -> leqSize' v v2) vs -- all v in vs <= v2
         (_,VMax _)  -> addLeq v1 v2 -- this produces a disjunction
         (VSucc v1,VSucc v2) -> leqSize' v1 v2
         (VGen v1,VGen v2) -> do
           d <- getSizeDiff v1 v2
           case d of
             Nothing -> throwErrorMsg $ "leqSize: head mismatch: " ++ show v1 ++ " !<= " ++ show v2
             Just k -> if k >= 0 then return () else throwErrorMsg $ "leqSize: " ++ show v1 ++ " !<= " ++ show v2 ++ " failed"
         (_,VInfty) -> return ()
         (VMeta i n, VSucc v2) | n > 0 -> leqSize' (VMeta i (n-1)) v2
         (VMeta i n, VMeta j m) -> addLeq (VMeta i (n - min n m))
                                          (VMeta j (m - min n m))
         (VMeta i n, v2) -> addLeq v1 v2
         (VSucc v1, VMeta i n) | n > 0 -> leqSize' v1 (VMeta i (n-1))
         (v1,VMeta i n) -> addLeq v1 v2
         (v1,VSucc v2) -> leqSize' v1 v2
         _ -> throwErrorMsg $ "leqSize: " ++ show v1 ++ " !<= " ++ show v2
-}

-- measures and guards -----------------------------------------------

{-
-- compare lexicographically
-- precondition: same length
ltMeasure :: Measure Val -> Measure Val -> TypeCheck ()
ltMeasure  (Measure mu1) (Measure mu2) =
  -- enter ("checking " ++ show mu1 ++ " < " ++ show mu2) $
    lexSizes Lt mu1 mu2
-}

{-
leqMeasure :: Pol -> Measure Val -> Measure Val -> TypeCheck ()
leqMeasure mixed (Measure mu1) (Measure mu2) = do
  zipWithM (leqSize mixed) mu1 mu2
  return ()
leqMeasure Pos (Measure mu1) (Measure mu2) = lexSizes mu1 mu2
leqMeasure Neg (Measure mu1) (Measure mu2) = lexSizes mu2 mu1
-}

-- lexSizes True  mu mu' checkes mu <  mu'
-- lexSizes False mu mu' checkes mu <= mu'
lexSizes :: LtLe -> [Val] -> [Val] -> TypeCheck ()
lexSizes ltle mu1 mu2 = traceSize ("lexSizes " ++ show (ltle,mu1,mu2)) $
  case (ltle, mu1, mu2) of
    (Lt, [], []) -> recoverFail $ "lexSizes: no descent detected"
    (Le, [], []) -> return ()
    (lt, a1:mu1, a2:mu2) -> do
      b <- newAssertionHandling Failure $ errorToBool $ leSize ltle Pos a1 a2
      case (lt,b) of
        (Le,False) -> recoverFailDoc $ text "lexSizes: expected" <+> prettyTCM a1 <+> text "<=" <+> prettyTCM a2
            -- recoverFail $ "lexSizes: expected " ++ show a1 ++ " <= " ++ show a2
        (Lt,True) -> return ()
        _ -> lexSizes ltle mu1 mu2

{-
      r <- compareSize a1 a2
      case r of
        LT -> return ()
        EQ -> lexSizes ltle mu1 mu2
        GT -> recoverFail $ "lexSizes: expected " ++ show a1 ++ " <= " ++ show a2
-}

{-
-- TODO: reprogram leqSize in terms of a proper compareSize
compareSize :: Val -> Val -> TypeCheck Ordering
compareSize a1 a2 = do
  let ret o = trace ("compareSize: " ++ show a1 ++ " compared to " ++ show a2 ++ " returns " ++ show o) $ return o
  le <- newAssertionHandling Failure $ errorToBool $ leqSize Pos a1 a2
  ge <- newAssertionHandling Failure $ errorToBool $ leqSize Pos a2 a1
  case (le,ge) of
    (True,False) -> ret LT -- THIS IS COMPLETE BOGUS!!!
    (True,True)  -> ret EQ
    (False,True) -> ret GT
    (False,False) -> fail $ "compareSize (" ++ show a1 ++ ", " ++ show a2 ++ "): sizes incomparable"
-}

{- Bound entailment

1. (mu1 <  mu1') ==> (mu2 <  mu2') if mu2 <= mu1 and mu1' <= mu2'
2. (mu1 <= mu1') ==> (mu2 <  mu2') one of these <= strict (<)
3. (mu1 <  mu1') ==> (mu2 <= mu2') as 1.
4. (mu1 <= mu1') ==> (mu2 <= mu2') as 1.

-}
entailsGuard :: Pol -> Bound Val -> Bound Val -> TypeCheck ()
entailsGuard pol beta1@(Bound ltle1 (Measure mu1) (Measure mu1')) beta2@(Bound ltle2 (Measure mu2) (Measure mu2')) = enterDoc (text ("entailsGuard:") <+> prettyTCM beta1 <+> text (show pol ++ "==>") <+> prettyTCM beta2) $ do
  case pol of
    _ | pol == mixed -> do
      assert (ltle1 == ltle2) $ "unequal bound types"
      zipWithM (leqSize mixed) mu1  mu2
      zipWithM (leqSize mixed) mu1' mu2'
      return ()
    Pos | ltle1 == Lt || ltle2 == Le  -> do
      lexSizes Le mu2  mu1  -- not strictly smaller
      lexSizes Le mu1' mu2'
      return ()
    Pos -> do
      (lexSizes Lt mu2  mu1 >> lexSizes Le mu1' mu2')
      `orM`
      (lexSizes Le mu2  mu1 >> lexSizes Lt mu1' mu2')
    Neg   -> entailsGuard (switch pol) beta2 beta1

{-
eqGuard :: Bound Val -> Bound Val -> TypeCheck ()
eqGuard (Bound (Measure mu1) (Measure mu1')) (Bound (Measure mu2) (Measure mu2')) = do
  zipWithM (leqSize mixed) mu1 mu2
  zipWithM (leqSize mixed) mu1' mu2'
  return ()
-}

checkGuard :: Bound Val -> TypeCheck ()
checkGuard beta@(Bound ltle mu mu') =
  enterDoc (text "checkGuard" <+> prettyTCM beta) $
    lexSizes ltle (measure mu) (measure mu')

addOrCheckGuard :: Pol -> Bound Val -> TypeCheck a -> TypeCheck a
addOrCheckGuard Neg beta cont = checkGuard beta >> cont
addOrCheckGuard Pos beta cont = addBoundHyp beta cont

-- comparing polarities -------------------------------------------------

leqPolM :: Pol -> PProd -> TypeCheck ()
leqPolM p (PProd Pol.Const _) = return ()
leqPolM p (PProd q m) | Map.null m && not (isPVar p) =
  if leqPol p q then return ()
   else recoverFail $ "polarity check " ++ show p ++ " <= " ++ show q ++ " failed"
leqPolM p q = do
  traceM $ "adding polarity constraint " ++ show p ++ " <= " ++ show q

leqPolPoly :: Pol -> PPoly -> TypeCheck ()
leqPolPoly p (PPoly l) = mapM_ (leqPolM p) l

-- adding an edge to the positivity graph
addPosEdge :: DefId -> DefId -> PProd -> TypeCheck ()
addPosEdge src tgt p = unless (src == tgt && isSPos p) $ do
  -- traceM ("adding interesting positivity graph edge  " ++ show src ++ " --[ " ++ show p ++ " ]--> " ++ show tgt)
  st <- get
  put $ st { positivityGraph = Arc (Rigid src) (ppoly p) (Rigid tgt) : positivityGraph st }

checkPositivityGraph :: TypeCheck ()
checkPositivityGraph = enter ("checking positivity") $ do
  st <- get
  let cs = positivityGraph st
  let gr = buildGraph cs
  let n  = nextNode gr
  let m0 = mkMatrix n (graph gr)
  let m  = warshall m0
  let isDataId i = case Map.lookup i (intMap gr) of
                     Just (Rigid (DefId DatK _)) -> True
                     _ -> False
  let dataDiag = [ m Array.! (i,i) | i <- [0..n-1], isDataId i ]
  mapM_ (\ x -> leqPolPoly oone x) dataDiag
{-
  let solvable = all (\ x -> leqPol oone x)
  unless solvable $ recoverFail $ "positivity check failed"
-}
  -- TODO: solve constraints
  put $ st { positivityGraph = [] }

-- telescopes --------------------------------------------------------

telView :: TVal -> TypeCheck ([(Val, TBinding TVal)], TVal)
telView tv = do
  case tv of
    VQuant Pi x dom fv -> underAbs_ x dom fv $ \ _ xv bv -> do
      (vTel, core) <- telView bv
      return ((xv, TBind x dom) : vTel, core)
    _ -> return ([], tv)

-- | Turn a fully applied constructor value into a named record value.
mkConVal :: Dotted -> ConK -> QName -> [Val] -> TVal -> TypeCheck Val
mkConVal dotted co n vs vc = do
  (vTel, _) <- telView vc
  let fieldNames = map (boundName . snd) vTel
  return $ VRecord (NamedRec co n False dotted) $ zip fieldNames vs

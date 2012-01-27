{-# LANGUAGE TupleSections, NamedFieldPuns #-}

module Extract where

{- extract to Fomega

Examples:
---------

MiniAgda

  data Vec (A : Set) : Nat -> Set
  { vnil  : Vec A zero
  ; vcons : [n : Nat] -> (head : A) -> (tail : Vec A n) -> Vec A (suc n) 
  } fields head, tail

  fun length : [A : Set] -> [n : Nat] -> Vec A n -> <n : Nat>
  { length .A .zero    (vnil A)         = zero
  ; length .A .(suc n) (vcons A n a as) = suc (length A n as)
  } 

Fomega

  data Vec (A : Set) : Set
  { vnil  : Vec A
  ; vcons : (head : A) -> (tail : Vec A) -> Vec A
  } 

  fun head : [A : Set] -> Vec A -> A
  { head (vcons 'head 'tail) = 'head
  }

  fun tail : [A : Set] -> Vec A -> A
  { head (vcons 'head 'tail) = 'tail
  }

  fun length : [A : Set] -> Vec A -> Nat
  { length [A]  vnil             = zero
  ; length [A] (vcons [.A] a as) = suc (length [A] as)
  } 


Bidirectional extraction
========================

Types 

  Base ::= D As         data type
         | ?            inexpressible type 
                        
  A,B ::= Base | A -> B | [x:K] -> B | [] -> B  with erasure markers
  A0, B0 ::= Base | A0 -> B0 | [x:K0] -> B0     without erasure markers

  |.| erase erasure markers

Inference mode:

  Term extraction:  Gamma |- t :> A  --> e    |Gamma| |- e : |A|
  Type extraction:  Gamma |- T :> K  --> A    |Gamma| |- A : |K|
  Kind extraction:  Gamma |- U :> [] --> K    |Gamma| |- K : []

Checking mode:

  Term extraction:  Gamma |- t <: A  --> e    |Gamma| |- e : |A|
  Type extraction:  Gamma |- T <: K  --> A    |Gamma| |- A : |K|
  Kind extraction:  Gamma |- U <: [] --> K    |Gamma| |- K : []

Type and kind extraction keep erasure markers!
  
Checking abstraction:

  Relevant abstraction:
  Gamma, x:A |- t <: B --> e
  --------------------------------
  Gamma |- \x.t <: A -> B --> \x.e

  Type abstraction:
  Gamma, x:K |- t <: B --> e : B0
  ----------------------------------------
  Gamma |- \[x].t <: [x:K] -> B --> \[x].e
      also \xt

  Irrelevant abstraction:
  Gamma |- t : B --> e
  -------------------------------
  Gamma |- \[x].t : [] -> B --> e
      also \xt

  Relevant abstraction at unknown type:
  Gamma, x:? |- t : ? --> e
  --------------------------
  Gamma |- \x.t : ? --> \x.e
  
  Irrelevant abstraction at unknown type:
  Gamma |- t : ? --> e
  -------------------------
  Gamma |- \[x].t : ? --> e
  
Checking by inference:
  
  Gamma |- t :> A --> e    e : |A| <: |B| --> e'
  ----------------------------------------------
  Gamma |- t <: B --> e' : B0 

Casting:

  ------------------ A0 does not contain ?
  e : A0 <: A0 --> e

  ----------------------- A0 != B0 or one does contain ?
  e : A0 <: B0 --> cast e

Inferring variable:
  
  ----------------------------
  Gamma |- x :> Gamma(x) --> x

Inferring application:

  Relevant application:
  Gamma |- t :> A -> B --> f     Gamma |- u <: A --> e
  ----------------------------------------------------
  Gamma |- t u :> B --> f e

  Type application:
  Gamma |- t :> [x:K] -> B --> f   Gamma |- u <: K --> A
  ------------------------------------------------------
  Gamma |- t [u] :> : B[A/x] --> f [A] 
      also  t u 
  
  Irrelevant application:
  Gamma |- t :> [] -> B --> f
  ---------------------------
  Gamma |- t [u] :> B --> f
      also  t u

  Relevant application at unknown type:
  Gamma |- t :> ? --> f     Gamma |- u <: ? --> e
  -----------------------------------------------
  Gamma |- t u :> ? --> f e

  Irrelevant application at unknown type:
  Gamma |- t :> ? --> f
  -------------------------
  Gamma |- t [u] :> ? --> f



-}

import Data.Char

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Data.Traversable (Traversable) 
import qualified Data.Traversable as Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Text.PrettyPrint

import Polarity as Pol
import Abstract
import Value
import Eval
import TCM
import TraceError
import Util

traceExtrM s = return ()

runExtract sig k = runErrorT (runReaderT (runStateT k (initWithSig sig)) emptyContext)

-- extraction

type FExpr        = Expr
type FDeclaration = Declaration
type FClause      = Clause
type FPattern     = Pattern
type FConstructor = Constructor
type FTypeSig     = TypeSig
type FFun         = Fun
type FTelescope   = Telescope

type FTVal        = TVal

extractDecls :: [EDeclaration] -> TypeCheck [FDeclaration]
extractDecls ds = concat <$> mapM extractDecl ds

extractDecl :: EDeclaration -> TypeCheck [FDeclaration]
extractDecl d = 
  case d of
    MutualDecl _ ds -> extractDecls ds -- TODO! 
    OverrideDecl{} -> fail $ "extractDecls internal error: overrides impossible"
    MutualFunDecl _ co funs -> extractFuns co funs
    FunDecl co fun -> extractFun co fun 
    LetDecl evl x [] (Just t) e -> extractLet evl x t e
    PatternDecl{}    -> return []
    DataDecl n _ co _ tel ty cs fields -> extractDataDecl n co tel ty cs

extractFuns :: Co -> [Fun] -> TypeCheck [FDeclaration]
extractFuns co funs = do
  funs <- concat <$> mapM extractFunTypeSig funs
  concat <$> mapM (extractFun co) funs

extractFun :: Co -> Fun -> TypeCheck [FDeclaration]
extractFun co (Fun (TypeSig n t) n' ar cls) = do
  tv <- whnf' t
  cls <- concat <$> mapM (extractClause n tv) cls
  return [ FunDecl co $ Fun (TypeSig n t) n' ar cls
         -- , LetDecl False (TypeSig n' t) (Var n)  -- no longer needed, since n and n' print the same
         ]

{- OLD
extractFun :: Co -> Fun -> TypeCheck [FDeclaration]
extractFun co (TypeSig n t, (ar, cls)) = extractIfTerm n $ do
  tv0 <- whnf' t
  t <- extractType tv0
  setExtrTyp n t
  let n' = mkExtName n
  setExtrTyp n' t
  tv <- whnf' t
  cls <- concat <$> mapM (extractClause n tv) cls
  return [ FunDecl co (TypeSig n t, (ar, cls))
         , LetDecl False (TypeSig n' t) (Var n)
         ]
-}
{-
extractFunTypeSigs :: [Fun] -> TypeCheck [Fun]
extractFunTypeSigs = mapM extractFunTypeSig
-}

-- only extract type sigs
extractFunTypeSig :: Fun -> TypeCheck [Fun]
extractFunTypeSig (Fun ts@(TypeSig n t) n' ar cls) = extractIfTerm n $ do
  ts@(TypeSig n t) <- extractTypeSig ts
  setExtrTyp n' t
  return [Fun ts n' ar cls]

extractLet :: Bool -> Name -> Type -> Expr -> TypeCheck [FDeclaration]
extractLet evl n t e = extractIfTerm n $ do
  TypeSig n t <- extractTypeSig (TypeSig n t)
  e <- extractCheck e =<< whnf' t
  return [LetDecl evl n [] (Just t) e] 
  
extractTypeSig :: TypeSig -> TypeCheck FTypeSig
extractTypeSig (TypeSig n t) = do
  t <- extractType =<< whnf' t
  setExtrTyp n t
  return $ TypeSig n t

extractIfTerm :: Name -> TypeCheck [a] -> TypeCheck [a]
extractIfTerm n cont = do
  k <- symbolKind <$> lookupSymb n
  if k == NoKind || lowerKind k == SortC Tm then cont else return []

extractDataDecl :: Name -> Co -> Telescope -> Type -> [Constructor] -> TypeCheck [FDeclaration]
extractDataDecl n co tel ty cs = do
  -- k    <- extrTyp <$> lookupSymb n
  tel' <- extractKindTel tel
  Just core <- addBinds tel $ extractKind =<< whnf' ty 
  -- (_, core) = typeToTele' (length tel') k 
  cs   <- mapM (extractConstructor tel) cs  
  return [DataDecl n NotSized co [] tel' core cs []]
  
extractConstructor :: Telescope -> Constructor -> TypeCheck FConstructor
extractConstructor tel0 (TypeSig n t) = do
{- fails for HEq
  -- 2012-01-22: remove irrelevant parameters
  let tel = filter (\ (TBind _ dom) -> not $ erased $ decor dom)  tel0
-}
  let tel = tel0
  -- compute full extracted constructor type and add to the signature
  t' <- extractType =<< whnf emptyEnv (teleToTypeErase tel t) 
  setExtrTyp n t'
  let (tel',core) = typeToTele' (length tel) t'
  return $ TypeSig n core
  -- compute type minus telescope
  -- TypeSig n <$> (extractType =<< whnf' t)

extractClause :: Name -> FTVal -> Clause -> TypeCheck [FClause]
extractClause f tv (Clause _ pl Nothing) = return [] -- discard absurd clauses
extractClause f tv cl@(Clause vtel pl (Just rhs)) = do
  traceM ("extracting clause " ++ render (prettyClause f cl)  
          ++ "\n at type " ++ showVal tv)
{-
  tel <- introPatterns pl tv0 $ \ _ _ -> do
           vtel <- getContextTele
           extractTeleVal vtel
  addBinds tel $ 
-}
  introPatVars pl $
    extractPatterns tv pl $ \ pl tv -> do
      rhs <- extractCheck rhs tv
      return [Clause vtel pl (Just rhs)] -- TODO: return FTelescope (type!)

-- the pattern variables are already in context
extractPatterns :: FTVal -> [Pattern] -> 
                   ([FPattern] -> FTVal -> TypeCheck a) -> TypeCheck a
extractPatterns tv [] cont = cont [] tv
extractPatterns tv (p:ps) cont = 
  extractPattern tv p $ \ pl tv -> 
    extractPatterns tv ps $ \ ps tv -> 
      cont (pl ++ ps) tv

extractPattern :: FTVal -> Pattern -> 
                  ([FPattern] -> FTVal -> TypeCheck a) -> TypeCheck a
extractPattern tv p cont = do
  traceM ("extracting pattern " ++ render (pretty p) ++ " at type " ++ showVal tv)
  fv <- funView tv
  case fv of
    EraseArg tv -> cont [] tv  -- skip erased patterns

    Forall x dom env t -> do
      xv <- whnf' (patternToExpr p) -- pattern variables are already in scope
      bv <- whnf (update env x xv) t -- TODO!
      case p of
        ErasedP (VarP y) -> setTypeOfName y dom $ cont [] bv
        _ -> cont [] bv
{-
    Forall x ki env t -> new x ki $ \ xv ->
      cont [] =<< whnf (update env x xv) t -- TODO!
-}
    Arrow av bv -> extractPattern' av p (flip cont bv)

extractPattern' :: FTVal -> Pattern -> 
                  ([FPattern] -> TypeCheck a) -> TypeCheck a
extractPattern' av p cont =
      case p of
        VarP y -> setTypeOfName y (defaultDomain av) $ 
          cont [VarP y]
        PairP p1 p2 -> do
          view <- prodView av
          -- hack to avoid IMPOSSIBLE
          let (av1, av2) = case view of
                             Prod av1 av2 -> (av1, av2)
                             _ -> (av, av) -- HACK
          extractPattern' av1 p1 $ \ ps1 -> do
            extractPattern' av2 p2 $ \ ps2 -> 
               let ps [] ps2    = ps2
                   ps ps1 []    = ps1
                   ps [p1] [p2] = [PairP p1 p2]
               in  cont $ ps ps1 ps2
            
{-
          case view of
            Prod av1 av2 ->
              extractPattern' av1 p1 $ \ [p1] -> do
                extractPattern' av2 p2 $ \ [p2] -> cont [PairP p1 p2]
            _ -> fail $ "extractPattern': IMPOSSIBLE: pattern " ++ 
                          show p ++ " : " ++ show av
-}
        ConP pi n ps -> do
--          tv <- whnf' =<< extrTyp <$> lookupSymb n
          tv <- extrConType n av
          extractPatterns tv ps $ \ ps _ ->
            cont [ConP pi n ps]
        _ -> cont []

extrConType :: Name -> FTVal -> TypeCheck FTVal
extrConType c av = do
  ConSig { numPars, extrTyp } <- lookupSymb c
  traceExtrM ("extrConType " ++ show c ++ " has extrTyp = " ++ show extrTyp)
  tv <- whnf' extrTyp
  case numPars of
   0 -> return tv
   _ -> do
    case av of
      VApp (VDef (DefId DatK d)) vs -> do
        DataSig { positivity } <- lookupSymb d
        traceExtrM ("extrConType " ++ show c ++ "; data type has positivity = " ++ show positivity)
        let pars 0 pols vs = []
            pars n (pol:pols) vs | erased pol = VIrr : pars (n-1) pols vs
            pars n (pol:pols) (v:vs) = v : pars (n-1) pols vs
            pars n pols vs = error $ "pars " ++ show n ++ show pols ++ show vs
        piApps tv $ pars numPars positivity $ vs ++ repeat VIrr
{-
        let (pars, inds) = splitAt numPars vs
        piApps tv pars
-}
      _ -> piApps tv $ replicate numPars VIrr
--      _ -> fail $ "extrConType " ++ show c ++ ": expected datatype, found " ++ show av

-- extracting a term from a term -------------------------------------

extractInfer :: Expr -> TypeCheck (FExpr, FTVal)
extractInfer e = do
  case e of

    Var x -> (Var x,) . typ . domain <$> lookupName1 x

    App f e0 -> do
      let (er, e) = isErasedExpr e0
      (f, tv) <- extractInfer f
      fv <- funView tv
      case fv of
        EraseArg bv -> return (f,bv)
        Forall x dom env b -> do 
          e <- extractTypeAt e (typ dom)
          bv <- (\ xv -> whnf (update env x xv) b) =<< whnf' e 
          return $ (App f (erasedExpr e), bv) 
        Arrow av bv -> return (if er then f else App f e, bv)
        NotFun -> return (if er then f else castExpr f `App` e, VIrr)
 
    Def f -> (Def f,) <$> do (whnf' . extrTyp) =<< lookupSymb (name f)
 
    Pair{} -> fail $ "extractInfer: IMPOSSIBLE: pair " ++ show e
    -- other expressions are erased or types

    _ -> return (Irr, VIrr)

extractCheck :: Expr -> FTVal -> TypeCheck (FExpr)
extractCheck e tv = do
  case e of
    Lam dec y e -> do
      fv <- funView tv
      case fv of
        EraseArg bv        -> extractCheck e bv -- discard lambda
        Forall x dom env b -> 
          Lam (decor dom) y <$> do
            newWithGen y dom $ \ i _ -> 
              extractCheck e =<< whnf (update env x (VGen i)) b
        Arrow av bv        ->
          if erased dec then extractCheck e bv
           else Lam dec y <$> do
             new' y (defaultDomain av) $
               extractCheck e bv
        NotFun            -> castExpr <$> 
          if erased dec then extractCheck e VIrr
           else Lam dec y <$> do
             new' y (defaultDomain VIrr) $
               extractCheck e VIrr

    LLet (TBind x dom0) [] e1 e2 -> do
      let dom = fmap Maybe.fromJust dom0 
      if erased (decor dom) then extractCheck e2 tv else do -- discard let
       vdom <- Traversable.mapM whnf' dom         -- MiniAgda type val
       dom  <- Traversable.mapM extractType vdom  -- Fomega type
       vdom <- Traversable.mapM whnf' dom         -- Fomega type val
       e1  <- extractCheck e1 (typ vdom)
       LLet (TBind x (fmap Just dom)) [] e1 <$> do
         new' x vdom $ extractCheck e2 tv

    Pair e1 e2 -> do
      view <- prodView tv
      let (av1,av2) = case view of
                        Prod av1 av2 -> (av1, av2)
                        _ -> (tv,tv) -- HACK!!
      Pair <$> extractCheck e1 av1 <*> extractCheck e2 av2
{-
      case view of
        Prod av1 av2 -> Pair <$> extractCheck e1 av1 <*> extractCheck e2 av2
        _ -> fail $ "extractCheck: tuple type expected " ++ show e ++ " : " ++ show tv
-}

    -- TODO: case

    _ -> fallback
  where 
    fallback = do
      (e,tv') <- extractInfer e
      insertCast e tv tv'

insertCast :: FExpr -> FTVal -> FTVal -> TypeCheck FExpr
insertCast e tv1 tv2 = loop tv1 tv2 where
  loop tv1 tv2 =
    case (tv1,tv2) of
      (VIrr,_) -> return $ castExpr e
      (_,VIrr) -> return $ castExpr e
      _  -> return e -- TODO!
 
funView :: FTVal -> TypeCheck FunView
funView tv = 
  case tv of
    -- erasure mark
    VQuant Pi x dom env e | erased (decor dom) && typ dom == VIrr -> 
      EraseArg <$> whnf (update env x VIrr) e
    -- forall
    VQuant Pi x dom env e | erased (decor dom) -> 
      return $ Forall x dom env e
    -- function type
    VQuant Pi x dom env e ->
      Arrow (typ dom) <$> whnf (update env x VIrr) e
    -- any other type can be a function type, but this needs casts!
    _ -> return NotFun -- $ Arrow VIrr VIrr

data FunView 
  = Arrow    FTVal FTVal            -- A -> B
  | Forall   Name Domain Env FType  -- forall X:K. A
  | EraseArg FTVal                  -- [] -> B
  | NotFun                          -- ()

prodView :: FTVal -> TypeCheck ProdView
prodView tv =
  case tv of
    VQuant Sigma x dom env b -> Prod (typ dom) <$> whnf (update env x VIrr) b
    _                        -> return $ NotProd

data ProdView
  = Prod FTVal FTVal -- A * B
  | NotProd

-- extracting a kind from a value ------------------------------------

type FKind = Expr -- FKind ::= Set | FKind -> FKind | [Irr] -> FKind

star :: FKind
star = Sort $ Set Zero

extractSet :: Sort Val -> Maybe FKind
extractSet s = 
  case s of
    SortC _ -> Nothing
    Set _   -> Just $ star
    CoSet _ -> Just $ star

-- keep irrelevant entries
extractKindTel :: Telescope -> TypeCheck FTelescope
extractKindTel [] = return []
extractKindTel (TBind x dom : tel) = do
  dom  <- Traversable.mapM whnf' dom
  dom' <- extractKindDom dom 
  if erased (decor dom') then
    newIrr x $  
      (TBind x dom' :) <$> extractKindTel tel
   else newTyVar x (typ dom') $ \ i -> do
      x <- nameOfGen i
      (TBind x dom' :) <$> extractKindTel tel 

{-
-- keep irrelevant entries
extractKindTel :: Telescope -> TypeCheck FTelescope
extractKindTel tel = do
  tv     <- whnf' (teleToType tel star)
  Just k <- extractKind tv
  let (tel, s) = typeToTele k
  return tel
  -- throw away erasure marks
  -- return $ filter (\ tb -> not $ erased $ decor $ boundDom tb) tel
-}

extractKindDom :: Domain -> TypeCheck (Dom FKind)
extractKindDom dom = 
  maybe (defaultIrrDom Irr) defaultDomain <$>
    if erased (decor dom) then return Nothing 
     else extractKind (typ dom)

extractKind :: TVal -> TypeCheck (Maybe FKind)
extractKind tv = 
  case tv of
    VSort s -> return $ extractSet s
    VMeasured mu vb -> extractKind vb
    VGuard beta vb -> extractKind vb
    VQuant Pi x dom env b -> new' x dom $ do
       bv  <- whnf (update env x VIrr) b
       mk' <- extractKind bv
       case mk' of
         Nothing -> return Nothing     
         Just k' -> do
           dom' <- extractKindDom dom
           let x = fresh ""
           return $ Just $ Quant Pi (TBind x dom') k' 
    _ -> return Nothing

-- extracting a type constructor from a value ------------------------

type FType = Expr 
{- FType ::= Irr                 -- not expressible in Fomega
           | D FTypes            -- data type
           | X FTypes            -- type variable
           | FType -> FType      -- function type
           | [X:FKind] -> FType  -- polymorphic type
           | [Irr] -> FType      -- erasure marker
 -}

-- tyVarName i = fresh $ "a" ++ show i

newTyVar :: Name -> FKind -> (Int -> TypeCheck a) -> TypeCheck a
newTyVar x k cont = newWithGen x (defaultDomain (VClos emptyEnv k)) $ 
  \ i _ -> cont i                  -- store kinds unevaluated

addFKindTel :: FTelescope -> TypeCheck a -> TypeCheck a
addFKindTel [] cont = cont
addFKindTel (TBind x dom : tel) cont = newTyVar x (typ dom) $ const $ 
  addFKindTel tel cont 

extractTeleVal :: TeleVal -> TypeCheck FTelescope
extractTeleVal [] = return []
extractTeleVal (tb : vtel) = do
  tb <- Traversable.mapM extractType tb
  addBind tb $ do 
    (tb :) <$> extractTeleVal vtel

extractType :: TVal -> TypeCheck FType
extractType = extractTypeAt star

extractTypeAt :: FKind -> TVal -> TypeCheck FType
extractTypeAt k tv = do
  case (tv,k) of

    (VMeasured mu vb, _) -> extractTypeAt k vb
    (VGuard beta vb, _) -> extractTypeAt k vb

    -- relevant function space / sigma type --> non-dependent
    (VQuant piSig x dom env b,_) | not (erased (decor dom)) -> do
      a <- extractType (typ dom) 
      -- new' x dom $ do
      bv <- whnf (update env x VIrr) b
      b  <- extractType bv 
      let x = fresh ""
      return $ Quant piSig (TBind x (defaultDomain a)) b

    -- irrelevant function space --> forall or erasure marker  
    (VQuant Pi x dom env b,_) | erased (decor dom) -> do
      mk <- extractKind (typ dom)
      case mk of
        Nothing -> do -- new' x dom $ do
          bv <- whnf (update env x VIrr) b
          b  <- extractType bv 
          let x = fresh ""
          return $ Quant Pi (TBind x (defaultIrrDom Irr)) b
        Just k' -> do
          newTyVar x k' $ \ i -> do
            bv <- whnf (update env x (VGen i)) b
            b  <- extractType bv 
            x  <- nameOfGen i
            return $ Quant Pi (TBind x (defaultIrrDom k')) b 

    (VApp (VDef (DefId DatK n)) vs, _) -> do
      k  <- extrTyp <$> lookupSymb n  -- get kind of dname from signature
      as <- extractTypes k vs  -- turn vs into types as at kind k
      return $ foldl App (Def (DefId DatK n)) as

    (VGen i,_) -> do     
--      VClos _ k <- (typ . fromOne . domain) <$> lookupGen i  -- get kind of var from cxt
      Var <$> nameOfGen i
      -- return $ Var (tyVarName i)

    (VApp (VGen i) vs,_) -> do     
      VClos _ k <- (typ . fromOne . domain) <$> lookupGen i  -- get kind of var from cxt
      as <- extractTypes k vs  -- turn vs into types as at kind k
      x <- nameOfGen i
      return $ foldl App (Var x) as

    (VLam x env e, Quant Pi (TBind _ dom) k) | erased (decor dom) -> do 
      tv <- whnf (update env x VIrr) e
      extractTypeAt k tv 
 
    (VLam x env e, Quant Pi (TBind _ dom) k) -> newTyVar x (typ dom) $ \ i -> do
      tv <- whnf (update env x (VGen i)) e
      x  <- nameOfGen i
      Lam defaultDec x <$> extractTypeAt k tv 
 
    (VLam{},_) -> error $ "panic! extractTypeAt " ++ show (tv,k)
 
    (VSing _ tv,_) -> extractTypeAt k tv 
 
    (VUp v _,_)    -> extractTypeAt k v 
 
    _ -> return Irr
 
extractTypes :: FKind -> [TVal] -> TypeCheck [FType]
extractTypes k vs = 
  case (k,vs) of
    (_, []) -> return []
    (Quant Pi (TBind _ dom) k, v:vs) | erased (decor dom) -> extractTypes k vs
    (Quant Pi (TBind _ dom) k, v:vs) -> do
      v  <- whnfClos v
      a  <- extractTypeAt (typ dom) v
      as <- extractTypes k vs
      return $ a : as
    _ -> error $ "panic! extractTypes  " ++ show k ++ "  " ++ show vs

-- auxiliary functions -----------------------------------------------

{- this is setExtrTyp
addFTypeSig :: Name -> FType -> TypeCheck ()
addFTypeSig n t = modifySig n (\ item -> item { extrTyp = t })
-}

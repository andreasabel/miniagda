module ToHaskell where

{- type-directed extraction of Haskell programs with a lot of unsafeCoerce

Examples:
---------

MiniAgda

  data Vec (A : Set) : Nat -> Set
  { vnil  : Vec A zero
  ; vcons : [n : Nat] -> (head : A) -> (tail : Vec A n) -> Vec A (suc n)
  }

  fun length : [A : Set] -> [n : Nat] -> Vec A n -> <n : Nat>
  { length .A .zero    (vnil A)         = zero
  ; length .A .(suc n) (vcons A n a as) = suc (length A n as)
  }

Haskell

  {-# LANGUAGE NoImplicitPrelude #-}
  module Main where
  import qualified Text.Show as Show

  data Vec (a :: *)
    = Vec_vnil
    | Vec_vcons { vec_head :: a , vec_tail :: Vec a }
      deriving Show.Show

  length :: forall a. Vec a -> Nat
  length  Vec_vnil        = Nat_zero
  length (Vec_vcons a as) = Nat_suc (length as)

Components:
-----------

Translation from MiniAgda identifiers to Haskell identifiers

-}

import Prelude hiding (null)

import Data.Char

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State  (StateT, evalStateT)

import qualified Data.Traversable as Trav

import qualified Language.Haskell.Exts.Syntax as Hs

import Polarity
import Abstract
import Extract
import qualified HsSyntax as H
import TraceError
import Util

-- translation monad

type Translate = StateT TState (ReaderT TContext (ExceptT TraceError IO))

{- no longer needed with mtl-2
instance Applicative Translate where
  pure      = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }
-}

data TState = TState

initSt :: TState
initSt = TState

data TContext = TContext

initCxt :: TContext
initCxt = TContext

runTranslate :: Translate a -> IO (Either TraceError a)
runTranslate t = runExceptT (runReaderT (evalStateT t initSt) initCxt)

-- translation

translateModule :: [EDeclaration] -> Translate (H.Module)
translateModule ds = do
  hs <- translateDecls ds
  return $ H.mkModule hs

translateDecls :: [EDeclaration] -> Translate [H.Decl]
translateDecls ds = concat <$> mapM translateDecl ds

translateDecl :: EDeclaration -> Translate [H.Decl]
translateDecl d =
  case d of
    MutualDecl _ ds -> translateDecls ds
    OverrideDecl{} -> throwErrorMsg $ "translateDecls internal error: overrides impossible"
    MutualFunDecl _ _ funs -> translateFuns funs
    FunDecl _ fun -> translateFun fun
    LetDecl _ x tel (Just t) _unfolds e | null tel -> translateLet x t e
    DataDecl n _ _ _ tel fkind cs _ -> translateDataDecl n tel fkind cs

translateFuns :: [Fun] -> Translate [H.Decl]
translateFuns funs = concat <$> mapM translateFun funs

translateFun :: Fun -> Translate [H.Decl]
translateFun (Fun ts _n _ar _unfolds cls) = do
  ts@(Hs.TypeSig _ [n] t) <- translateTypeSig ts
  cls <- concat <$> mapM (translateClause n) cls
  return [ts, H.hFunBind cls]

translateLet :: Name -> Type -> FExpr -> Translate [H.Decl]
translateLet n t e
  | isEtaAlias n = return []  -- skip internal decls
  | otherwise = do
      ts <- translateTypeSig $ TypeSig n t
      e  <- translateExpr e
      n  <- hsName (DefId LetK $ QName n)
      return [ ts, H.mkLet n e ]

translateTypeSig :: TypeSig -> Translate H.Decl
translateTypeSig (TypeSig n t) = do
  n <- hsName (DefId LetK $ QName n)
  t <- translateType t
  return $ H.mkTypeSig n t

translateDataDecl :: Name -> FTelescope -> FKind -> [FConstructor] -> Translate [H.Decl]
translateDataDecl n tel k cs = do
  n   <- hsName (DefId DatK $ QName n)
  tel <- translateTelescope tel
  let k' = translateKind k
  cs  <- mapM translateConstructor cs
  return [H.mkDataDecl n tel k' cs]

translateConstructor :: FConstructor -> Translate H.GadtDecl
translateConstructor (Constructor n pars t) = do
  n  <- hsName (DefId (ConK Cons) n)
  t' <- translateType t
  return $ H.mkConDecl n t'

translateClause :: H.Name -> Clause -> Translate [H.Match]
translateClause n (Clause _ ps (Just rhs)) = do
  ps <- mapM translatePattern ps
  rhs <- translateExpr rhs
  return [H.mkClause n ps rhs]

translateTelescope :: FTelescope -> Translate [H.TyVarBind]
translateTelescope (Telescope tel) = mapM translateTBind tel'
  -- throw away erasure marks
  where tel' = filter (\ tb -> not $ erased $ decor $ boundDom tb) tel

translateTBind :: TBind -> Translate H.TyVarBind
translateTBind (TBind x dom) = do
  x <- hsVarName x
  return $ H.hKindedVar x $ translateKind (typ dom)

translateKind :: FKind -> H.Kind
translateKind k =
  case k of
    k | k == star -> H.hKindStar
    Quant Pi (TBind _ dom) k' | erased (decor dom) -> translateKind k'
    Quant Pi (TBind _ dom) k' ->
      translateKind (typ dom) `H.mkKindFun` translateKind k'

translateType :: FType -> Translate H.Type
translateType t =
  case t of

    Irr -> return $ H.unit_tycon

    Quant piSig (TBind _ dom) b | not (erased (decor dom)) ->
      H.mkTyPiSig piSig <$> translateType (typ dom) <*> translateType b

    Quant Pi (TBind _ dom) b | typ dom == Irr -> translateType b

    Quant Pi (TBind x dom) b -> do
      x <- hsVarName x
      let k = translateKind (typ dom)
      -- todo: add x to context
      t <- translateType b
      return $ H.mkForall x k t

    App f a -> H.mkTyApp <$> translateType f <*> translateType a

    Def d@(DefId DatK n) -> (H.hTyCon . H.hUnQual) <$> hsName d

    Var x -> H.hTyVar <$> hsVarName x

    _ -> return H.unit_tycon

{- TODO:
    _ -> throwErrorMsg $ "no Haskell representation for type " ++ show t
 -}

translateExpr :: FExpr -> Translate H.Exp
translateExpr e =
  case e of

    Var x -> H.mkVar <$> hsVarName x

    -- constructors
    Def f@(DefId (ConK{}) n) -> H.mkCon <$> hsName f

    -- function identifiers
    Def f@(DefId _ n) -> H.mkVar <$> hsName f

    -- discard type arguments
    App f e0 -> do
      f <- translateExpr f
      let (er, e) = isErasedExpr e0
      if er then return f else H.mkApp f <$> translateExpr e

    -- discard type lambdas
    Lam dec y e -> do
      y <- hsVarName y
      e <- translateExpr e
      return $ if erased dec then e else H.mkLam y e

    LLet (TBind x dom) tel e1 e2 | null tel-> do
      x  <- hsVarName x
      e2 <- translateExpr e2
      if erased (decor dom) then return e2 else do
        t  <- Trav.mapM translateType (typ dom)
        e1 <- translateExpr e1
        return $ H.mkLLet x t e1 e2

    Pair e1 e2 -> H.mkPair <$> translateExpr e1 <*> translateExpr e2

    -- TODO

    Ann (Tagged [Cast] e) -> H.mkCast <$> translateExpr e

    _ -> return $ Hs.unit_con ()

translatePattern :: Pattern -> Translate H.Pat
translatePattern p =
  case p of
    VarP y       -> H.hPVar <$> hsVarName y
    PairP p1 p2  -> H.hPTuple Hs.Boxed <$> mapM translatePattern [p1,p2]
    ConP pi n ps ->
       H.hPApp <$> (H.hUnQual <$> hsName (DefId (ConK $ coPat pi) n))
               <*> mapM translatePattern ps

{-
Name translation

  data names        : check capitalization, identity translation
  constructor names : prefix with Dataname_
  destructor names  : ditto
  type-valued lets  : check capitalization, identity
  type-valued funs  : reject!
  lets              : check lowercase
  funs/cofuns       : check lowercase
-}

hsVarName :: Name -> Translate H.Name
hsVarName x = return $ H.hIdent $ show x

hsName :: DefId -> Translate H.Name
hsName id = enter ("error translating identifier " ++ show id) $
  case id of
  (DefId DatK (QName x)) -> do
    let n = suggestion x
    unless (isUpper $ head n) $
      throwErrorMsg $ "data names need to be capitalized"
    return $ H.hIdent n
  (DefId (ConK co) (Qual d x)) -> do
    let n = suggestion x
        m = suggestion d
    return $ H.hIdent $ m ++ "_" ++ n
    -- dataName <- getDataName x
    -- return $ H.hIdent $ dataName ++ "_" ++ n
  -- lets, funs, cofuns. TODO: type-valued funs!
--   (DefId Let ('_':n)) | -> return $ H.hIdent n
  (DefId _ x) -> do
    let n = suggestion $ unqual x
{- ignore for now
     unless (isLower $ head n) $
       throwErrorMsg $ "function names need to start with a lowercase letter"
 -}
    return $ H.hIdent n

-- getDataName constructorName = return dataNamec
getDataName :: Name -> Translate String
getDataName n = return "DATA"

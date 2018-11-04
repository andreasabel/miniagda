{- 2010-09-17 haskell syntax tools -}

module HsSyntax where

import Abstract (PiSigma(..))
-- import Language.Haskell.Exts.SrcLoc (SrcLoc(..))
import Language.Haskell.Exts.Syntax ()
import qualified Language.Haskell.Exts.Syntax as H

-- noLoc :: SrcLoc
-- noLoc = SrcLoc "" 0 0

noLoc :: ()
noLoc = ()

type Exp        = H.Exp ()
type Pat        = H.Pat ()
type Type       = H.Type ()
type Kind       = H.Kind ()
type Decl       = H.Decl ()
type Name       = H.Name ()
type QName      = H.QName ()
type Match      = H.Match ()
type Module     = H.Module ()
type ImportDecl = H.ImportDecl ()
type TyVarBind  = H.TyVarBind ()
type GadtDecl   = H.GadtDecl ()

hQual         = H.Qual ()
hUnQual       = H.UnQual ()
hModuleName   = H.ModuleName ()
hIdent        = H.Ident ()
hFunBind      = H.FunBind ()
hVar          = H.Var ()
hCon          = H.Con ()
hLit          = H.Lit ()
hApp          = H.App ()
hString s     = H.String () s s
hTyVar        = H.TyVar ()
hTyCon        = H.TyCon ()
hTyApp        = H.TyApp ()
hDataType     = H.DataType ()
hKindFn       = H.KindFn ()
hTyTuple      = H.TyTuple ()
hTyFun        = H.TyFun ()
hTyForall     = H.TyForall ()
hKindedVar    = H.KindedVar ()
hTyParen      = H.TyParen ()
hBDecls       = H.BDecls ()
hUnGuardedRhs = H.UnGuardedRhs ()
hParen        = H.Paren ()
hLet          = H.Let ()
hTuple        = H.Tuple ()
hPVar         = H.PVar ()
hPApp         = H.PApp ()
hPTuple       = H.PTuple ()
hKindStar     = H.KindStar ()

main_name  = H.main_name ()
main_mod   = H.main_mod ()
unit_tycon = H.unit_tycon ()

mkQual :: String -> String -> QName
mkQual m s = hQual (hModuleName m) (hIdent s)

mkModule :: [Decl] -> Module
mkModule hs = H.Module noLoc (Just (H.ModuleHead () main_mod warning exports)) pragmas imports decls where
  pragmas = [ H.LanguagePragma noLoc $ map hIdent
    [ "NoImplicitPrelude"
    , "GADTs"
    , "KindSignatures"
    ]]
  warning = Nothing
  exports = Nothing
  imports =
    [ mkQualImport "GHC.Show" "Show"
    , mkQualImport "System.IO" "IO"
    , mkQualImport "Unsafe.Coerce" "Coerce"
    ]
  decls   = hs ++
    [ H.TypeSig noLoc [ main_name ] io
    , hFunBind [ mkClause main_name [] rhs ]
    ] where rhs  = hVar (mkQual "IO" "putStrLn") `hApp` hLit (hString "Hello, world!")
            io   = hTyCon (mkQual "IO" "IO") `hTyApp` unit_tycon

mkQualImport :: String -> String -> ImportDecl
mkQualImport modName asName =
  H.ImportDecl
  { H.importAnn       = noLoc
  , H.importModule    = hModuleName modName
  , H.importQualified = True
  , H.importSrc       = False
  , H.importPkg       = Nothing
  , H.importAs        = Just $ hModuleName asName
  , H.importSpecs     = Nothing
  }

noContext = Nothing
noDeriving = []
noTyVarBind = []
showDeriving = H.Deriving ()
  (Just (H.DerivStock ()))
  [H.IRule () Nothing Nothing $ H.IHCon () (mkQual "Show" "Show")]

mkDataDecl :: Name -> [TyVarBind] -> Kind -> [GadtDecl] -> Decl
mkDataDecl n tel k cs =
  H.GDataDecl noLoc hDataType noContext
    (foldl (H.DHApp ()) (H.DHead () n) tel)
    (Just k) cs [showDeriving]

mkConDecl :: Name -> Type -> GadtDecl
mkConDecl n t = H.GadtDecl noLoc n Nothing t

mkKindFun :: Kind -> Kind -> Kind
mkKindFun = hKindFn
{-
mkKindFun k k' = parens k `KindFn` k'
      where parens H.KindStar = H.KindStar
            parens k          = H.KindParen k
-}

mkTyPiSig :: PiSigma -> Type -> Type -> Type
mkTyPiSig Pi    = mkTyFun
mkTyPiSig Sigma = mkTyProd

mkTyProd :: Type -> Type -> Type
mkTyProd a b = hTyTuple H.Boxed [a,b]

mkTyFun :: Type -> Type -> Type
mkTyFun = hTyFun
-- mkTyFun a b = mkTyParen a `TyFun` b

mkForall :: Name -> Kind -> Type -> Type
mkForall x k t = hTyForall (Just $ [hKindedVar x k]) noContext t

mkTyParen :: Type -> Type
mkTyParen a@(H.TyVar{}) = a
mkTyParen a@(H.TyCon{}) = a
mkTyParen a = hTyParen a

mkTyApp :: Type -> Type -> Type
mkTyApp f a = hTyApp f a

noBinds = hBDecls []

mkTypeSig :: Name -> Type -> Decl
mkTypeSig x t = H.TypeSig noLoc [x] t

-- create a simple function clause x = t
mkLet :: Name -> Exp -> Decl
mkLet x e = hFunBind [mkClause x [] e]

mkClause :: Name -> [Pat] -> Exp -> Match
mkClause f ps e = H.Match noLoc f ps (hUnGuardedRhs e) Nothing

mkCast :: Exp -> Exp
mkCast e = hVar (mkQual "Coerce" "unsafeCoerce") `hApp` e

mkCon :: Name -> Exp
mkCon = hCon . hUnQual

mkVar :: Name -> Exp
mkVar = hVar . hUnQual

mkLam :: Name -> Exp -> Exp
mkLam x (H.Lambda _ ps e) = H.Lambda noLoc (hPVar x : ps) e
mkLam x  e                = H.Lambda noLoc [hPVar x] e

mkParen :: Exp -> Exp
mkParen e@(H.Var{}) = e
mkParen e@(H.Con{}) = e
mkParen e = hParen e

mkApp :: Exp -> Exp -> Exp
mkApp f e = hApp f e -- (mkParen e)

mkLLet :: Name -> Maybe Type -> Exp -> Exp -> Exp
mkLLet x t e e' = hLet (hBDecls [mkLet x e]) e'

mkPair :: Exp -> Exp -> Exp
mkPair e1 e2 = hTuple H.Boxed [e1,e2]

{- this is already predefined as unit_con
hsDummyExp :: HsExp
hsDummyExp = HsCon $ Special $ HsUnitCon  -- Haskell's '()'
-}

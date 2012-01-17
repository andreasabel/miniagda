{- 2010-09-17 haskell syntax tools -}

module HsSyntax where

import Abstract (PiSigma(..))
import Language.Haskell.Exts.Syntax

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

mkQual :: String -> String -> QName
mkQual m s = Qual (ModuleName m) (Ident s)

mkModule :: [Decl] -> Module
mkModule hs = Module noLoc main_mod pragmas warning exports imports decls where
  pragmas = [ LanguagePragma noLoc $ map Ident
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
    [ TypeSig noLoc [ main_name ] io
    , FunBind [ mkClause main_name [] rhs ]
    ] where rhs  = Var (mkQual "IO" "putStrLn") `App` Lit (String "Hello, world!")
            io   = TyCon (mkQual "IO" "IO") `TyApp` unit_tycon

mkQualImport :: String -> String -> ImportDecl
mkQualImport modName asName =
  ImportDecl 
  { importLoc       = noLoc 
  , importModule    = ModuleName modName
  , importQualified = True 
  , importSrc       = False 
  , importPkg       = Nothing
  , importAs        = Just $ ModuleName asName
  , importSpecs     = Nothing
  } 

noContext = []
noDeriving = []
noTyVarBind = []
showDeriving = (mkQual "Show" "Show", [])

mkDataDecl :: Name -> [TyVarBind] -> Kind -> [GadtDecl] -> Decl
mkDataDecl n tel k cs = GDataDecl noLoc DataType noContext n tel (Just k) cs [showDeriving]

mkConDecl :: Name -> Type -> GadtDecl
mkConDecl n t = GadtDecl noLoc n t

mkKindFun :: Kind -> Kind -> Kind
mkKindFun = KindFn
{-
mkKindFun k k' = parens k `KindFn` k'
      where parens H.KindStar = H.KindStar
            parens k          = H.KindParen k
-}

mkTyPiSig :: PiSigma -> Type -> Type -> Type
mkTyPiSig Pi    = mkTyFun
mkTyPiSig Sigma = mkTyProd

mkTyProd :: Type -> Type -> Type
mkTyProd a b = TyTuple Boxed [a,b]

mkTyFun :: Type -> Type -> Type
mkTyFun = TyFun
-- mkTyFun a b = mkTyParen a `TyFun` b

mkForall :: Name -> Kind -> Type -> Type
mkForall x k t = TyForall (Just $ [KindedVar x k]) noContext t

mkTyParen :: Type -> Type
mkTyParen a@(TyVar{}) = a
mkTyParen a@(TyCon{}) = a
mkTyParen a = TyParen a

mkTyApp :: Type -> Type -> Type
mkTyApp f a = TyApp f a

noBinds = BDecls []

mkTypeSig :: Name -> Type -> Decl
mkTypeSig x t = TypeSig noLoc [x] t

-- create a simple function clause x = t
mkLet :: Name -> Exp -> Decl
mkLet x e = FunBind [mkClause x [] e]

mkClause :: Name -> [Pat] -> Exp -> Match
mkClause f ps e = Match noLoc f ps Nothing (UnGuardedRhs e) noBinds

mkCast :: Exp -> Exp
mkCast e = Var (mkQual "Coerce" "unsafeCoerce") `App` e

mkCon :: Name -> Exp
mkCon = Con . UnQual

mkVar :: Name -> Exp
mkVar = Var . UnQual

mkLam :: Name -> Exp -> Exp
mkLam x (Lambda _ ps e) = Lambda noLoc (PVar x : ps) e
mkLam x  e              = Lambda noLoc [PVar x] e

mkParen :: Exp -> Exp
mkParen e@(Var{}) = e
mkParen e@(Con{}) = e
mkParen e = Paren e

mkApp :: Exp -> Exp -> Exp
mkApp f e = App f e -- (mkParen e) 

mkLLet :: Name -> Type -> Exp -> Exp -> Exp
mkLLet x t e e' = Let (BDecls [mkLet x e]) e'

mkPair :: Exp -> Exp -> Exp
mkPair e1 e2 = Tuple [e1,e2]

{- this is already predefined as unit_con
hsDummyExp :: HsExp
hsDummyExp = HsCon $ Special $ HsUnitCon  -- Haskell's '()'
-}

module HsSrcExtTests where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

mkModule :: [Decl] -> Module
mkModule hs = Module noLoc main_mod pragmas warning exports imports decls where
  pragmas = []
  warning = Nothing
  exports = Nothing
  imports = []
  decls   = hs

noContext = []
showDeriving = (UnQual (Ident "Show"), [])

mkDataDecl :: Name -> [TyVarBind] -> Kind -> [GadtDecl] -> Decl
mkDataDecl n tel k cs = GDataDecl noLoc DataType noContext n tel (Just k) cs [showDeriving]

mkConDecl :: Name -> Type -> GadtDecl
mkConDecl n t = GadtDecl noLoc n t

noBinds = BDecls []

mkClause :: Name -> [Pat] -> Exp -> Match
mkClause f ps e = Match noLoc f ps Nothing (UnGuardedRhs e) noBinds

mkId x = UnQual $ Ident $ x
mkCon c = Con $ UnQual $ Ident $ c
mkJust = mkCon "Just"
mkNothing = mkCon "Nothing"

hsmodule = mkModule
  [ mkDataDecl (Ident "Unit") [] KindStar
      [ mkConDecl (Ident "Unit") (TyCon $ UnQual $ Ident "Unit") ]
  , FunBind [ mkClause (Ident "bla")
      [ PApp (mkId "Just")
      [ PApp (mkId "Just")
      [ PApp (mkId "Nothing") [
      ] ] ] ] $
        App mkJust (App mkJust mkNothing)
      ]
  ]

main :: IO ()
main = putStrLn $ prettyPrint hsmodule

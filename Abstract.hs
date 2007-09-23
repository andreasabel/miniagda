-- syntax
module Abstract where

type Name = String

data Co = Ind 
        | CoInd
          deriving (Eq,Show)

data Expr = Set
          -- size type
          | Size 
          | Succ Expr
          | Infty
          -- 
          | Var Name
          | Con Name
          | Def Name
          | Const Name
          | App Expr [Expr]
          | Lam Name Expr
          | Pi Name Expr Expr
          | Ident Name -- not used after scope checking
          deriving (Eq)

instance Show Expr where
    show = prettyExpr

data Declaration = DataDecl Name Co Telescope Type [Constructor]
                 | FunDecl Co [(TypeSig,[Clause])] -- may be mutually recursive
                 | ConstDecl TypeSig Expr
                   deriving (Eq,Show)

data TypeSig = TypeSig Name Type
             deriving (Eq,Show)

type Type = Expr

type Constructor = TypeSig

type TBind = (Name,Type)

type Telescope = [TBind]

data Clause = Clause [Pattern] Expr  
            deriving (Eq,Show)

data Pattern = VarP Name
             | ConP Name [Pattern]
             | SuccP Pattern
             | DotP Expr
             | IdentP Name -- not used after scope checking
               deriving (Eq,Show)

teleToType :: Telescope -> Type -> Type
teleToType [] t = t
teleToType ((n,t):tel) t2 = Pi n t (teleToType tel t2)

splitTeleType :: Int -> (Telescope,Type) -> (Telescope,Type)
splitTeleType 0 (tel,t) = (tel,t)
splitTeleType k (tel,(Pi n t t2)) = splitTeleType (k - 1) (tel ++ [(n,t)],t2) 

typeToTele :: Type -> (Telescope, Type)
typeToTele t = ttt t []
    where 
      ttt :: Type -> Telescope -> (Telescope,Type)
      ttt (Pi n t' t2) tel = ttt t2 (tel ++ [(n,t')])
      ttt x tel = (tel,x)                          

----

prettyExpr :: Expr -> String
prettyExpr e = 
    case e of
      Set -> "Set"
      Size -> "Size" 
      Succ e -> "(s " ++ prettyExpr e ++ ")"
      Infty -> "infty"
      Var n -> n
      Con n -> n
      Def n -> n
      Const n -> n
      App e1 el -> "(" ++ prettyExprs (e1:el) ++ ")"
      Lam x e1 -> "(\\" ++ x ++ " -> " ++ prettyExpr e1
      Pi "" t1 t2 -> "(" ++ prettyExpr t1 ++ " -> " ++ prettyExpr t2 ++ ")" 
      Pi x t1 t2 -> "( ( " ++ x ++ " : " ++ prettyExpr t1 ++ ") -> " ++ prettyExpr t2 ++ ")"
      Ident n -> n
                                                                                            

prettyExprs :: [Expr] -> String
prettyExprs [] = ""
prettyExprs (e:es) = prettyExpr e ++ (if null es then "" else " " ++ prettyExprs es)

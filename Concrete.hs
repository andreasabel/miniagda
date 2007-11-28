-- concrete syntax
module Concrete where

import Abstract (Name,Co,Pos,Sized)

data Expr = Set
          -- size type
          | Size 
          | Succ Expr
          | Infty
          -- 
          | App Expr [Expr]
          | Lam Name Expr
          | LLet Name Expr Expr Expr -- local let
          | Pi Name Expr Expr
          | Ident Name 
          deriving (Eq)

instance Show Expr where
    show = prettyExpr

data Declaration = DataDecl Name Sized Co Telescope Type [Constructor]
                 | FunDecl Co TypeSig [Clause] 
                 | LetDecl Bool TypeSig Expr 
                 | MutualDecl [Declaration] -- bool = if eval
                   deriving (Eq,Show)

data TypeSig = TypeSig Name Type
             deriving (Eq,Show)

type Type = Expr

type Constructor = TypeSig

data TBind = TB Name Type 
    | PosTB Name Type
      deriving(Eq,Show)

type Telescope = [TBind]

data Clause = Clause [Pattern] Expr  
            deriving (Eq,Show)

data Pattern = ConP Name [Pattern]
             | SuccP Pattern
             | DotP Expr
             | IdentP Name 
               deriving (Eq,Show)

----



prettyExpr :: Expr -> String
prettyExpr e = 
    case e of
      Set -> "Set"
      Size -> "Size" 
      Succ e -> "($ " ++ prettyExpr e ++ ")"
      Infty -> "#"
      App e1 el -> "(" ++ prettyExprs (e1:el) ++ ")"
      Lam x e1 -> "(\\" ++ x ++ " -> " ++ prettyExpr e1 ++ ")"
      LLet n t1 e1 e2 -> "(let " ++ n ++ " : " ++ prettyExpr t1 ++ " = " ++ prettyExpr e1 ++ " in " ++ prettyExpr e2 ++ ")" 
      Pi "" t1 t2 -> "(" ++ prettyExpr t1 ++ " -> " ++ prettyExpr t2 ++ ")" 
      Pi x t1 t2 -> "( ( " ++ x ++ " : " ++ prettyExpr t1 ++ ") -> " ++ prettyExpr t2 ++ ")"
      Ident n -> n
                                                                                            

prettyExprs :: [Expr] -> String
prettyExprs [] = ""
prettyExprs (e:es) = prettyExpr e ++ (if null es then "" else " " ++ prettyExprs es)


teleToType :: Telescope -> Type -> Type
teleToType [] t = t
teleToType (TB n t:tel) t2 = Pi n t (teleToType tel t2)
teleToType (PosTB n t:tel) t2 = Pi n t (teleToType tel t2)

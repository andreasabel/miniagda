-- syntax
module Abstract where

type Name = String

data Sized = Sized | NotSized 
             deriving (Eq,Show)

data Co = Ind 
        | CoInd
          deriving (Eq,Show)

-- positivity 
data Pos = SPos | NSPos  
         deriving (Eq,Show)

data Expr = Set
          -- size type
          | Size 
          | Succ Expr
          | Infty
          -- 
          | Var Name
          | Con Co Name
          | Def Name
          | Let Name -- global let
          | LLet Name Expr Expr Expr --local let
          | App Expr [Expr]
          | Lam Name Expr
          | Pi Name Expr Expr
          deriving (Eq)

instance Show Expr where
    show = prettyExpr

data Declaration = DataDecl Name Sized Co [Pos] Telescope Type [Constructor]
                 | FunDecl [(TypeSig,Co,[Clause])]  -- may be mutual
                 | LetDecl Bool TypeSig Expr --bool for eval
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
             | ConP Co Name [Pattern]
             | SuccP Pattern
             | DotP Expr
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
      Succ e -> "($ " ++ prettyExpr e ++ ")"
      Infty -> "#"
      Var n -> n
      Con _ n -> n
      Def n -> n
      Let n -> n
      LLet n t1 e1 e2 ->
          "(let " ++ n ++ " : " ++ prettyExpr t1 ++ " = " ++ prettyExpr e1 ++ " in " ++ prettyExpr e2 ++ ")"  
      App e1 el -> "(" ++ prettyExprs (e1:el) ++ ")"
      Lam x e1 -> "(\\" ++ x ++ " -> " ++ prettyExpr e1 ++ ")"
      Pi "" t1 t2 -> "(" ++ prettyExpr t1 ++ " -> " ++ prettyExpr t2 ++ ")" 
      Pi x t1 t2 -> "( ( " ++ x ++ " : " ++ prettyExpr t1 ++ ") -> " ++ prettyExpr t2 ++ ")"
                                                                                                 

prettyExprs :: [Expr] -> String
prettyExprs [] = ""
prettyExprs (e:es) = prettyExpr e ++ (if null es then "" else " " ++ prettyExprs es)

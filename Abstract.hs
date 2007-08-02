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
          | Pi TBind Expr
          | Fun Expr Expr
          | Ident Name -- not used after scope checking
          deriving (Eq,Show)

data Declaration = DataDecl Name Co Telescope Type [Constructor]
                 | FunDecl Co [(TypeSig,[Clause])] -- may be mutually recursive
                 | ConstDecl TypeSig Expr
                   deriving (Eq,Show)

data TypeSig = TypeSig Name Type
             deriving (Eq,Show)

type Type = Expr

type Constructor = TypeSig

data TBind = TBind Name Type
                    deriving (Eq,Show)

type Telescope = [TBind]

data Clause = Clause LHS RHS  
            deriving (Eq,Show)

data RHS = RHS Expr
         | AbsurdRHS
           deriving (Eq,Show)

data LHS = LHS [Pattern]
         deriving (Eq,Show)

data Pattern = VarP Name
             | ConP Name [Pattern]
             | SuccP Pattern
             | WildP
             | AbsurdP
             | IdentP Name -- not used after scope checking
               deriving (Eq,Show)

teleToType :: Telescope -> Type -> Type
teleToType [] t = t
teleToType (tb:tel) t = Pi tb (teleToType tel t)

typeToTele :: Type -> (Telescope, Type)
typeToTele t = ttt t []
    where 
      ttt :: Type -> Telescope -> (Telescope,Type)
      ttt (Pi tb t2) tel = ttt t2 (tel ++ [tb])
      ttt x tel = (tel,x)                          

----

arity :: Type -> Int
arity t = case t of 
            (Fun e1 e2) -> 1 + arity e2
            (Pi t e2) -> 1 + arity e2
            _ -> 0
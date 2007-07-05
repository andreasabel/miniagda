-- syntax
module Abstract where

type Name = String

data Expr = Set
          -- size type
          | Size 
          | Succ Expr
          | Infty
          -- 
          | Var Name
          | Con Name
          | Def Name
          | App Expr [Expr]
          | Lam TBind Expr
          | Pi TBind Expr
          | Fun Expr Expr
          | Ident Name -- not used after scope checking
          deriving (Eq,Show)

data Declaration = Declaration [TypeSig] [Definition] -- lists because of mutual definitons 
                   deriving (Eq,Show)

data Definition = DataDef Telescope [Constructor]
                | FunDef [Clause] -- also used for constant defintions, def t : T = e as a nullary function 
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


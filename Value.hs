module Value where

import Abstract

{-
data Clos = Clos Env Expr 
          deriving (Eq)
-}

data Val =   VSet
           | VSize
           | VInfty
           | VSucc Val   -- VSucc Env Expr
           | VApp Val [Clos]
--           | VApp Val [Val]
           | VCon Name
           | VDef Name
           | VGen Int
           | VClos Env Expr 
             deriving (Eq)
type Clos = Val 
type TVal = Val

instance Show Val where
    show = prettyVal

{-
prettyClos :: Clos -> String
prettyClos (Clos env e) = prettyEnv env ++ prettyExpr e

prettyClosures :: [Clos] -> String
prettyClosures [] = ""
prettyClosures (v:vs) = prettyClos v ++ (if null vs then "" else " " ++ prettyClosures vs)
-}

prettyClosures :: [Clos] -> String
prettyClosures = prettyVals

prettyVal :: Val -> String
prettyVal VSet = "Set"
prettyVal VSize = "Size"
prettyVal VInfty = "#"
prettyVal (VSucc v) = "($ " ++ prettyVal v ++ ")" 
prettyVal (VApp v vl) = "(" ++ prettyVal v ++ " " ++ prettyClosures vl ++ ")"
prettyVal (VCon n) = n
prettyVal (VDef n) = n
prettyVal (VGen k) = show k
prettyVal (VClos env e) = prettyEnv env ++ prettyExpr e

prettyVals :: [Val] -> String
prettyVals [] = ""
prettyVals (v:vs) = prettyVal v ++ (if null vs then "" else " " ++ prettyVals vs)

prettyEnv :: Env -> String
prettyEnv [] = ""
prettyEnv x = "{" ++ prettyEnv' x ++ "} " where
    prettyEnv' [] = []
    prettyEnv' ((n,v):env) = "(" ++ n ++ " = " ++ prettyVal v ++ ")" ++ prettyEnv' env 

----

type Env = [(Name,Val)]

emptyEnv = []

update :: Env -> Name -> Val -> Env
update env n v = (n,v):env

lookupEnv :: Env -> Name -> Val
lookupEnv [] n = error $ "lookup env error " ++ n 
lookupEnv ((x,v):xs) n = if x == n then v else lookupEnv xs n



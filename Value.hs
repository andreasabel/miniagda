module Value where

import Abstract

data Val =   VSet
           | VSize
           | VInfty
           | VSucc Val
           | VApp Val [Val]
           | VCon Name
           | VDef Name
           | VGen Int
           | VClos Env Expr 
             deriving (Eq)

type TVal = Val

instance Show Val where
    show = prettyVal

prettyVal :: Val -> String
prettyVal VSet = "Set"
prettyVal VSize = "Size"
prettyVal VInfty = "infty"
prettyVal (VSucc v) = "(s " ++ prettyVal v ++ ")" 
prettyVal (VApp v vl) = "(" ++ prettyVal v ++ " " ++ prettyVals vl ++ ")"
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
    prettyEnv' ((n,v):env) = "(" ++ n ++ " : " ++ prettyVal v ++ ")" ++ prettyEnv' env 

----

type Env = [(Name,Val)]

emptyEnv = []

update :: Env -> Name -> Val -> Env
update env n v = (n,v):env

lookupEnv :: Env -> Name -> Val
lookupEnv [] n = error $ "lookup error " ++ n 
lookupEnv ((x,v):xs) n = if x == n then v else lookupEnv xs n



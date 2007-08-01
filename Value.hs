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
           | VLam Name Env Expr
           | VPi  Name Val Env Expr
             deriving (Show)

prettyVal :: Val -> String
prettyVal VSet = "Set"
prettyVal VSize = "Size"
prettyVal (VSucc v) = "(s " ++ prettyVal v ++ ")" 
prettyVal (VApp v vl) = "(" ++ prettyVal v ++ " " ++ prettyVals vl ++ ")"
prettyVal (VCon n) = n
prettyVal (VDef n) = n
prettyVal x = show x

prettyVals :: [Val] -> String
prettyVals [] = ""
prettyVals (v:vs) = prettyVal v ++ (if null vs then "" else " " ++ prettyVals vs)

----

type Env = [(Name,Val)]

emptyEnv = []

update :: Env -> Name -> Val -> Env
update env n v = (n,v):env

lookupEnv :: Env -> Name -> Val
lookupEnv [] n = error $ "lookup error " ++ n 
lookupEnv ((x,v):xs) n = if x == n then v else lookupEnv xs n



module Value where

import Abstract

data Val = VGen Int
           | VApp Val [Val]
           | VSet
           | VSize
           | VInfty
           | VSucc Val
           | VClos Env Expr
           | VPi Name Expr Expr
           | VLam Name Expr
           | VCon Name
 
type Env = [(Name,Val)]

type FunContext = [(Name,(Int,[Clause]))] -- name, arity, clauses


lookupFunCtx :: FunContext -> Name -> (Int,[Clause])
lookupFunCtx ctx n = case (lookup n ctx) of
                       Nothing -> error $ "Error"
                       Just k -> k


type ConContext = [(Name,Int)] -- name,aritiy, definition 

lookupConContext :: ConContext -> Name -> Int
lookupConContext ctx n = case (lookup n ctx) of
                           Nothing -> error $ "Error"
                           Just k -> k

update :: Env -> Name -> Val -> Env
update env n v = (n,v):env

lookupEnv :: Env -> Name -> Val
lookupEnv [] n = error $ "lookup error " ++ n 
lookupEnv ((x,v):xs) n = if x == n then v else lookupEnv xs n


---- Pattern Matching ----

matches :: Pattern -> Val -> Bool
matches p v = case (p,v) of
                (VarP x,_) -> True
                (ConP x pl,VClos _ (App (Con n) al)) -> x == n && matchesList pl []
                (WildP ,_) -> True
                (SuccP _,_) -> matchesSucc p v
                _ -> False
                

matchesSucc :: Pattern -> Val -> Bool
matchesSucc p v = case (p,v) of
                    (SuccP (VarP x),VInfty) -> True
                    (SuccP (VarP x),VSucc v2) -> True
                    (SuccP p2,VSucc v2) -> matchesSucc p2 v2
                    _ -> False

matchesList :: [Pattern] -> [Val] -> Bool
matchesList [] [] = True
matchesList (p:pl) (v:vl) = matches p v && matchesList pl vl

upPattern :: Env -> Pattern -> Val -> Env
upPattern env p v = case (p,v) of
                      (VarP x,_) -> update env x v
                      (ConP x pl,VClos _ (App (Con n) al)) -> upPatterns env pl []
                      (WildP,_) -> env
                      (SuccP _,_) -> upSuccPattern env p v

upSuccPattern :: Env -> Pattern -> Val -> Env
upSuccPattern env p v = case (p,v) of                                              
                      (SuccP (VarP x),VInfty) -> update env x v
                      (SuccP (VarP x),VSucc v2) -> update env x v2
                      (SuccP p2,VSucc v2) -> upSuccPattern env p2 v2

upPatterns :: Env -> [Pattern] -> [Val] -> Env
upPatterns env [] [] = env
upPatterns env (p:pl) (v:vl) = let env' = upPattern env p v in
                               upPatterns env' pl vl


matchFun :: Name -> [Val] -> Val
matchFun n vl = 
    let (arity,cl) = lookupFunCtx [] n in
    if (arity == length vl) then
        matchClauses cl vl 
    else error "arity error mathfun"

matchClauses :: [Clause] -> [Val] -> Val
matchClauses [] vl = error "match error"
matchClauses (c:cl) vl = case c of 
                           (Clause (LHS pl) rhs) -> case matchClause [] pl rhs vl of
                                                      Nothing -> matchClauses cl vl
                                                      Just v -> v

matchClause :: Env -> [Pattern] -> RHS -> [Val] -> Maybe Val
matchClause env [] (RHS e) [] = Just (VClos env e)
matchClause env (p:pl) rhs (v:vl) = if (matches p v) then 
                                        matchClause (upPattern env p v) pl rhs vl
                                    else
                                        Nothing 


-----------------------

--- Interpreter


app :: Val -> [Val] -> Val
app u v = case u of
            VClos env (Lam (TBind x _) e) -> eval (update env x (head v)) e
            VClos env (Def n) -> matchFun n v
            _ -> VApp u v

eval :: Env -> Expr -> Val
eval env e = case e of
               Set -> VSet
               Size -> VSize
               Con n -> VCon n
               App e1 e2 -> app (eval env e1) (map (eval env) e2)
               _ -> VClos env e


module TypeChecker where

import Abstract
import Value
import Signature

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

-- reader monad for local environment
-- state monad for global signature
type TypeCheck a = ReaderT Env (StateT Signature Identity) a

runTypeCheck :: Env -> Signature -> TypeCheck a -> (a,Signature)
runTypeCheck env sig tc = runIdentity (runStateT (runReaderT tc env) sig)  
 
typeCheck dl = snd $ runTypeCheck emptyEnv emptySig (typeCheckDecls dl)

typeCheckDecls :: [Declaration] -> TypeCheck ()
typeCheckDecls [] = return ()
typeCheckDecls (d:ds) = do typeCheckDeclaration d
                           typeCheckDecls ds
                           return ()

typeCheckDeclaration :: Declaration -> TypeCheck ()
typeCheckDeclaration (DataDecl n co tel t cs) = 
    do sig <- get
       let dt = (teleToType tel t)
       put (addSig sig n (DataSig co dt (arity dt)))
       mapM (typeCheckConstructor tel) cs
       return ()
typeCheckDeclaration (FunDecl co funs) = typeCheckFuns co funs 
typeCheckDeclaration (ConstDecl (TypeSig n t) e ) = 
    do sig <- get
       _ <- if (typecheck sig e t ) then
                put (addSig sig n (ConstSig t (arity t) e))
            else 
                error $ "typecheck error const " ++ n
       return ()
typeCheckFuns :: Co -> [(TypeSig,[Clause])] -> TypeCheck ()
typeCheckFuns co [] = return ()
typeCheckFuns co ((TypeSig n t,cl):rest) = 
    do sig <- get
       put (addSig sig n (FunSig co t (arity t) cl))
       typeCheckFuns co rest
                         

typeCheckConstructor :: Telescope -> Constructor -> TypeCheck ()
typeCheckConstructor tel (TypeSig n t) = do sig <- get
                                            let tt = teleToType tel t  
                                            put (addSig sig n (ConSig tt (arity tt))) 
                                            return ()


---



-- check that patterns have the same length, return length
checkClauses :: Name -> [Clause] -> Int
checkClauses n [] = error $ "no clauses in " ++ n 
checkClauses n ((Clause (LHS pl) rhs):cl) = checkLength (length pl) cl 
    where 
      checkLength i [] = i
      checkLength i ((Clause (LHS pl) rhs):xs) = if (length pl) == i then checkLength i xs else error $ "checkClause " ++ n ++ " size of patterns differ" 
                                        
              
---- Pattern Matching ----

matches :: Pattern -> Val -> Bool
matches p v = case (p,v) of
                (VarP x,_) -> True
                (ConP x [],VCon y) -> x == y
                (ConP x pl,VApp (VCon y) vl) -> x == y && matchesList pl vl
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
matchesList _ _ = False
{-
matchesList x y = error $ "Error matchesList " ++ show x ++ " , " ++ show y 
-}

upPattern :: Env -> Pattern -> Val -> Env
upPattern env p v = case (p,v) of
                      (VarP x,_) -> update env x v
                      (ConP x [],VCon y) -> env
                      (ConP x pl,VApp (VCon y) vl) -> upPatterns env pl vl
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




matchClauses :: Name -> Signature -> Env -> [Clause] -> [Val] -> Val
matchClauses n sig env cl vl = loop cl
    where loop [] = error $ n ++ ": no function clause matches " ++ show vl
                    ++ "\n Clauses: " ++ show cl 
{-
                    ++ "\n Environment: " ++ show env
                    ++ "\n Signature: " ++ show sig 
 -}
          loop  ((Clause (LHS pl) rhs) : cl) = 
              case matchClause n sig env pl rhs vl of
                Nothing -> loop cl
                Just v -> v

matchClause :: Name -> Signature -> Env -> [Pattern] -> RHS -> [Val] -> Maybe Val
matchClause n sig env [] (RHS e) vl = Just (app sig (eval sig env e) vl)
matchClause n sig env (p:pl) rhs (v:vl) = 
    if (matches p v) then 
        matchClause n sig (upPattern env p v) pl rhs vl
    else
        Nothing 
matchClause n sig env pl _ [] = error $ "matchClause " ++ n 
-- ++ (show pl) ++ "\n" ++ (show vl) 
  ++ " (too few arguments) "


--- Interpreter

fapp :: Signature -> Env -> Name -> Val -> Expr -> [Val] -> Val
fapp sig env x v e vl = app sig (eval sig (update env x v) e) vl

app :: Signature -> Val -> [Val] -> Val

app sig u v = case (u,v) of
            (VApp u2 v2,_) -> app sig u2 (v2 ++ v)
            (_,[]) -> u
            (VLam x env e,(v:vl)) -> fapp sig env x v e vl
            (VPi  x _ env e,(v:vl)) -> fapp sig env x v e vl
            (VDef n,_) -> appDef sig [] n v

            _ -> VApp u v


appDef :: Signature -> Env -> Name -> [Val] -> Val
appDef sig env n vl = 
    case lookupSig n sig of
      (FunSig Ind   t arity cl) | arity     <= (length vl) -> matchClauses n sig env cl vl  
      (FunSig CoInd t arity cl) | arity + 1 <= (length vl) -> matchClauses n sig env cl vl  
      _ -> VApp (VDef n) vl   


vsucc :: Val -> Val
vsucc VInfty = VInfty
vsucc x = VSucc x

eval :: Signature -> Env -> Expr -> Val
eval sig env e = case e of
               Set -> VSet
               Infty -> VInfty
               Succ e -> vsucc (eval sig env e)
               Size -> VSize
               Con n -> VCon n
               App e1 e2 -> app sig (eval sig env e1) (map (eval sig env) e2)
               Def n -> VDef n
               Const n -> let (ConstSig t a e) = lookupSig n sig in eval sig env e 
               Var y -> lookupEnv env y
               Lam n e2 -> VLam n env e2
               Pi (TBind n t) t2 -> VPi n (eval sig env t) env t2
               Fun t1 t2 -> VPi "" (eval sig env t1) env t2

----

eqVal :: Signature -> Int -> Val -> Val -> Bool
eqVal sig k u1 u2 = 
    case (u1,u2) of
      (VSet,VSet) -> True
      (VSize,VSize) -> True
      (VInfty,VInfty) -> True
      (VSucc v1,VSucc v2) -> eqVal sig k v1 v2
      (VApp v1 w1,VApp v2 w2 ) -> eqVal sig k v1 v2 && eqVals sig k w1 w2
      (VGen k1,VGen k2) -> k1 == k2 || (error $ "gen mismatch "  ++ show k1 ++ " " ++ show k2 )
      (VPi x1 a1 env1 b1, VPi x2 a2 env2 b2) -> 
          let v = VGen k 
          in eqVal sig k a1 a2 && eqVal sig (k+1) (fapp sig env1 x1 v b1 []) (fapp sig env2 x2 v b2 [])
      (VLam x1 env1 b1, VLam x2 env2 b2) -> 
          let v = VGen k 
          in eqVal sig (k+1) (fapp sig env1 x1 v b1 []) (fapp sig env2 x2 v b2 [])
      (VDef x,VDef y) -> x == y || (error $ "eqVal VDef " ++ show x ++ " " ++ show y)
      _ -> error $ "eqVal error " ++ show u1 ++ " @@ " ++ show u2

eqVals :: Signature -> Int -> [Val] -> [Val] -> Bool
eqVals sig k [] [] = True
eqVals sig k (v1:vs1) (v2:vs2) = eqVal sig k v1 v2 && eqVals sig k vs1 vs2 
eqVals _ _ _ _ = False

-- type checking

checkExpr :: Signature -> Int -> Env -> Env -> Expr -> Val -> Bool
checkExpr sig k rho gamma e v = 
    case (e,v) of
      (Lam n e1,VPi x w env t2) -> 
          let v = VGen k
          in checkExpr sig (k+1) (update rho n v) (update gamma n w) e1 (fapp sig env x v t2 [])  
      (Pi (TBind n t1) t2,VSet) -> 
          checkType sig k rho gamma t1 &&
          checkType sig (k+1) (update rho n (VGen k)) (update gamma n (eval sig rho t1)) t2
      (Fun t1 t2,VSet) -> checkType sig k rho gamma t1 && checkType sig k rho gamma t2
      (Succ e2,VSize) -> checkExpr sig k rho gamma e2 v
      _ -> eqVal sig k (inferExpr sig k rho gamma e) v

inferExpr :: Signature -> Int -> Env -> Env -> Expr -> Val 
inferExpr sig k rho gamma e = 
    case e of
      Var x -> lookupEnv gamma x
      Set -> VSet
      Size -> VSet
      Infty -> VSize
      App e1 [e2] -> case (inferExpr sig k rho gamma e1) of
                          VPi n w env e3 ->  if (checkExpr sig k rho gamma e2 w) then
                                                (fapp sig env n (eval sig rho e2) e3 []) 
                                             else error "inferExpr : app error"
                          _ -> error "inferExp : expected Pi" 
      App e1 (e2:el) -> inferExpr sig k rho gamma (App (App e1 [e2]) el) -- hack ?  
      (Def n) -> case (lookupSig n sig) of
                   (DataSig co t a) -> eval sig rho t
                   (ConstSig t e a) -> eval sig rho t
                   (ConSig t a ) -> eval sig rho t
                   _ -> error $ "bla" ++ show n
      (Con n) -> case (lookupSig n sig) of
                   (ConSig t a) -> eval sig rho t
                   _ -> error $ "imposibble " 
      _ -> error $ "cannot infer type " ++ show e

      

checkType :: Signature -> Int -> Env -> Env -> Expr -> Bool
checkType sig k rho gamma e = checkExpr sig k rho gamma e VSet 


typecheck :: Signature -> Expr -> Expr -> Bool
typecheck sig e t = checkExpr sig 0 emptyEnv emptyEnv e (eval sig emptyEnv t)


-- type check a funtion

checkFun :: Signature -> Type -> [Clause] -> Bool
checkFun sig t cll =
    let i = getClauseNumPatterns cll
        (tel,rhstype) = splittype t i
    in all (checkClause sig tel rhstype) cll

checkClause :: Signature -> [TBind] -> Type -> Clause -> Bool
checkClause sig tel t (Clause (LHS pl) rhs) = 
    let (k,rho,gamma) = checkLHS sig tel pl
    in checkRHS sig k rho gamma rhs (eval sig rho t) 

checkLHS :: Signature -> [TBind] -> [Pattern] -> (Int,Env,Env)
checkLHS sig tel pl = loop 0 [] [] tel pl 
  where
    loop k env1 env2 [] [] = (k,env1,env2)
    loop k env1 env2 (tb:tel') (p:pl') = 
        let (k',env1',env2') = checkPattern sig k env1 env2 tb p 
        in
          loop k' env1' env2' tel' pl'

checkPattern :: Signature -> Int -> Env -> Env -> TBind -> Pattern -> (Int,Env,Env)
checkPattern sig k env1 env2 tb p = (k,env1,env2)

checkRHS :: Signature -> Int -> Env -> Env -> RHS -> Val -> Bool
checkRHS sig k rho gamma rhs v =
    case rhs of 
      (AbsurdRHS) -> True
      (RHS e) -> checkExpr sig k rho gamma e v  
      

splittype :: Type -> Int -> ([TBind],Type)
splittype t 0 = ([],t)
splittype (Fun t1 t2) k | k > 0 = let (l,t) = splittype t2 (k - 1) in (TBind "" t1:l,t) 
splittype (Pi tb t2) k | k > 0 = let (l,t) = splittype t2 (k - 1) in (tb:l,t) 

-- check that all clauses have equal number of patterns in left hand side
getClauseNumPatterns :: [Clause] -> Int
getClauseNumPatterns [] = error $ "typecheck error : empty clauses"
getClauseNumPatterns ((Clause (LHS pl) rhs) : xs) =
    let k = length pl 
    in checkNumPatterns k xs
       where 
         checkNumPatterns k [] = k
         checkNumPatterns k ((Clause (LHS pl) rhs):xs) = if length pl == k then checkNumPatterns k xs else error $
                                                         "pattern number mismatch"

-----
-- test

e1 :: Expr
e1 = Lam "A" (Lam "x" (Var "x"))

t1 :: Expr
t1 = Pi (TBind "A" Set) (Pi (TBind "y" (Var "A")) (Var "A"))

e2 = Succ Infty
t2 = Size


-- data type 

natSigdef :: SigDef
natSigdef = DataSig Ind Set 0

zeroSigdef :: SigDef
zeroSigdef = ConSig (Def "Nat") 0

succSigdef :: SigDef
succSigdef = ConSig (Fun (Def "Nat") (Def "Nat")) 1


bool = DataSig Ind Set 0

tt = ConSig (Def "Bool") 0
ff = ConSig (Def "Bool") 0

bla = DataSig Ind (Fun (Def "Nat") Set) 1

blub = ConstSig (Pi (TBind"x" (Def "Nat")) Set) 1 (Def "Bla") 

ftype :: Type
ftype = (Fun (Def "Bool") Set)

fcl1 :: Clause
fcl1 = (Clause (LHS [ConP "tt" []]) (RHS (Def "Nat")))

fcl2 = (Clause (LHS [ConP "ff" []]) (RHS (Def "Bool")))

f = FunSig Ind ftype 1 [fcl1,fcl2]


f2type :: Type
f2type = (Pi (TBind "x" (Def "Bool")) (App (Def "f") [Def "x"]))

f2cl1 = (Clause (LHS [ConP "tt" []]) (RHS (Con "zero"))) 

f2cl2 = (Clause (LHS [ConP "ff" []]) (RHS (Con "tt")))

f2 = FunSig Ind  f2type 1 [f2cl1,f2cl2]
      

signatur1 :: Signature
signatur1 = [("Nat",natSigdef),("zero",zeroSigdef),("succ",succSigdef)
            ,("Bla",bla),("Blub",blub),("f",f),("f2",f2)]

e3 = Def ("Blub")
t3 = Fun (Def "Nat") Set

e4 = Con ("zero")
t4 = Def ("Nat")

e5 = Def "succ"
t5 = Fun (Def "Nat") (Def "Nat")

e6 = (App (Def "succ") [e4])
t6 = Def "Nat"





test :: Bool
test = typecheck signatur1 e6 t6


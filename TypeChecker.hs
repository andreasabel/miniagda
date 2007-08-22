module TypeChecker where

import Abstract
import Value
import Signature

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace

-- reader monad for local environment (not used right now)
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
   
    do sig <-  trace ("TypeChecking data " ++ show n) get
       let dt = (teleToType tel t) 
       checkType 0 [] [] dt 
       put (addSig sig n (DataSig co dt))
       mapM (typeCheckConstructor n tel dt) cs
       return ()
typeCheckDeclaration (FunDecl co funs) = typeCheckFuns co funs 
typeCheckDeclaration (ConstDecl (TypeSig n t) e ) = 
    do sig <-  trace ("TypeChecking const " ++ show n) get
       checkType 0 [] [] t 
       vt <- eval [] t
       checkExpr 0 [] [] e vt
       put (addSig sig n (ConstSig t e))
       return ()

typeCheckFuns :: Co -> [(TypeSig,[Clause])] -> TypeCheck()
typeCheckFuns co funs = do typeCheckFunSig funs
                           typeCheckFunClause funs
  where
    typeCheckFunSig :: [(TypeSig,[Clause])] -> TypeCheck ()
    typeCheckFunSig [] = return ()
    typeCheckFunSig ((TypeSig n t,cl):rest) = 
        do sig <- trace ("TypeChecking type of " ++ show n) get
           ar <- arity co t cl
           put (addSig sig n (FunSig co t ar cl))
           typeCheckFunSig rest
    typeCheckFunClause :: [(TypeSig,[Clause])] -> TypeCheck ()
    typeCheckFunClause [] = return ()
    typeCheckFunClause ((TypeSig n t,cl):rest) =
        do sig <-  trace ("TypeChecking clauses of " ++ show n) get
           checkFun t cl 
           typeCheckFunClause rest         
                
typeCheckConstructor :: Name -> Telescope -> Type -> Constructor -> TypeCheck ()
typeCheckConstructor d tel target (TypeSig n t) = do sig <- trace ("TypeChecking constructor " ++ n) get 
                                                     let tt = teleToType tel t
                                                     checkType 0 [] [] tt
                                                     checkTarget d tel target tt 
                                                     put (addSig sig n (ConSig tt)) 
                                                     return ()

checkTarget :: Name -> Telescope -> Type -> Type -> TypeCheck()
checkTarget d tel dt c = return () -- TODO check constructor arguments and target

-- for inductive data types, the number of clauses is sufficient
arity :: Co -> Type -> [Clause] -> TypeCheck Int
arity co t cl = do x <- arityClauses cl 
                   y <- case x of
                          Nothing -> error $ "error : pattern length mismatch"
                          Just k -> case co of 
                                      Ind -> return $ k
                                      CoInd -> arityType t
                   return y

arityType :: Type -> TypeCheck Int
arityType t = do case t of 
                   (Fun e1 e2) -> do x <- arityType e2
                                     return $ x + 1 
                   (Pi t e2) -> do x <- arityType e2
                                   return $ x + 1
                   _ -> return 0

-- have to use this ?
arityVType :: Val -> TypeCheck Int
arityVType v = case v of
                 (VPi n v2 env e) -> do v3 <- eval env e 
                                        k <- arityVType v3 
                                        return $ k + 1
                 _ -> return 0

-- check that patterns have the same length, return length
arityClauses :: [Clause] -> TypeCheck (Maybe Int)
arityClauses [] = error $ "error : empty clause list"  
arityClauses ((Clause (LHS pl) rhs):cl) = checkLength (length pl) cl 
    where 
      checkLength i [] = return $ Just i
      checkLength i ((Clause (LHS pl) rhs):xs) = do let x =(length pl) 
                                                    case (x == i) of
                                                      True ->
                                                          checkLength i xs 
                                                      False ->
                                                        return Nothing 
                                        
              
---- Pattern Matching ----

matches :: Pattern -> Val -> Bool
matches p v = case (p,v) of
                (_,VGen _) -> False
                (VarP x,_) -> True
                (ConP x [],VCon y) -> x == y
                (ConP x pl,VApp (VCon y) vl) -> x == y && matchesList pl vl
                (WildP ,_) -> True
                (SuccP _,_) -> matchesSucc p v
                (DotP _,_) -> True
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

upPattern :: Env -> Pattern -> Val -> Env
upPattern env p v = case (p,v) of
                      (VarP x,_) -> update env x v
                      (ConP x [],VCon y) -> env
                      (ConP x pl,VApp (VCon y) vl) -> upPatterns env pl vl
                      (WildP,_) -> env
                      (SuccP _,_) -> upSuccPattern env p v
                      (DotP _,_) -> env

upSuccPattern :: Env -> Pattern -> Val -> Env
upSuccPattern env p v = case (p,v) of
                      (SuccP (VarP x),VInfty) -> update env x v
                      (SuccP (VarP x),VSucc v2) -> update env x v2
                      (SuccP p2,VSucc v2) -> upSuccPattern env p2 v2

upPatterns :: Env -> [Pattern] -> [Val] -> Env
upPatterns env [] [] = env
upPatterns env (p:pl) (v:vl) = let env' = upPattern env p v in
                               upPatterns env' pl vl




matchClauses :: Name -> [Clause] -> [Val] -> TypeCheck (Maybe Val)
matchClauses n cl vl = loop cl
    where loop [] = return Nothing
          loop  ((Clause (LHS pl) rhs) : cl2) = 
              do x <- matchClause n [] pl rhs vl 
                 case x of
                   Nothing -> loop cl2
                   Just v -> return $ Just v

matchClause :: Name -> Env -> [Pattern] -> RHS -> [Val] -> TypeCheck (Maybe Val)
matchClause n env [] (RHS e) vl = do v <- eval env e
                                     v2 <- app v vl
                                     return $ Just v2
matchClause n env (p:pl) rhs (v0:vl) =
  do v <- force v0
     let b = matches p v 
     case b of
       True ->
         matchClause n (upPattern env p v) pl rhs vl
       False -> return Nothing
matchClause n env pl _ [] = error $ "matchClause " ++ n ++ " " ++ (show pl)  ++ " (too few arguments) "

patternToExpr :: Pattern -> Expr
patternToExpr p =
    case p of
      VarP n -> Var n
      ConP n pl -> App (Con n) (map patternToExpr pl)
      SuccP p -> Succ (patternToExpr p)
      DotP e -> e
      WildP -> Var ""
      _ -> error $ "Pat to expr " ++ show p
--- Interpreter

fapp :: Env -> Name -> Val -> Expr -> [Val] -> TypeCheck Val
fapp env x v e vl = do v' <- eval (update env x v) e 
                       app v' vl

app :: Val -> [Val] -> TypeCheck Val
app u v = case (u,v) of
            (VApp u2 v2,_) -> app u2 (v2 ++ v)
            (VLam x env e,(v:vl)) -> fapp env x v e vl
            (VPi  x _ env e,(v:vl)) -> fapp env x v e vl
            (VDef n,_) -> appDef n v
            (_,[]) -> return u
            _ -> return $ VApp u v

-- unroll a corecursive object once
force :: Val -> TypeCheck Val
force v@ (VDef n) = 
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= 0 -> do m <- matchClauses n cl []
                                                      case m of
                                                        Nothing -> return v
                                                        Just v2 -> return v2
         _ -> return v 
force v@(VApp (VDef n) vl) =
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= (length vl) -> do m <- matchClauses n cl vl
                                                                case m of 
                                                                  Nothing -> return v
                                                                  Just v2 -> return v2
         _ -> return v
force v = return v

appDef :: Name -> [Val] -> TypeCheck Val
appDef n vl = 
    do
      sig <- get
      case lookupSig n sig of
        (FunSig Ind   t arity cl) | arity     <= (length vl) -> do m <- matchClauses n cl vl 
                                                                   case m of
                                                                     Nothing -> return $ VApp (VDef n) vl
                                                                     Just v2 -> return v2
        (FunSig CoInd t arity cl) | arity + 1 <= (length vl) ->  do m <- matchClauses n cl vl
                                                                    case m of
                                                                      Nothing -> return $ VApp (VDef n) vl
                                                                      Just v2 -> return v2
        _ -> case vl of 
               [] -> return $ VDef n  
               _ -> return $ VApp (VDef n) vl   


vsucc :: Val -> Val
vsucc VInfty = VInfty
vsucc x = VSucc x

eval :: Env -> Expr -> TypeCheck Val
eval env e = case e of
               Set -> return  VSet
               Infty -> return VInfty
               Succ e -> do v <- (eval env e)
                            return $ vsucc v
               Size -> return VSize
               Con n -> return $ VCon n
               App e1 e2 -> do v1 <- eval env e1
                               v2 <- mapM (eval env) e2
                               app v1 v2
               Def n -> return $ VDef n
               Const n -> do sig <- get 
                             let (ConstSig t e) = lookupSig n sig 
                             eval env e 
               Var y -> return $ lookupEnv env y
               Lam n e2 -> return $ VLam n env e2
               Pi (TBind n t) t2 -> do 
                      v <- eval env t
                      return $ VPi n v env t2
               Fun t1 t2 -> do 
                      v <- eval env t1
                      return $ VPi "" v env t2
               _ -> error $ "eval " ++ show e

runEval sig env e = fst $ runTypeCheck emptyEnv sig (eval env e)
----

eqVal :: Int -> Val -> Val -> TypeCheck Bool
eqVal k u1 u2 = 
    case (u1,u2) of
      (VSet,VSet) -> return True
      (VSize,VSize) -> return True
      (VInfty,VInfty) -> return True
      (VSucc v1,VSucc v2) -> eqVal k v1 v2
      (VApp v1 w1,VApp v2 w2 ) -> do eqVal k v1 v2 
                                     eqVals k w1 w2
      (VGen k1,VGen k2) -> return $ k1 == k2 || (error $ "gen mismatch "  ++ show k1 ++ " " ++ show k2 )
      (VPi x1 a1 env1 b1, VPi x2 a2 env2 b2) -> 
          do let v = VGen k 
             eqVal k a1 a2
             v1 <- fapp env1 x1 v b1 []
             v2 <- fapp env2 x2 v b2 []
             eqVal (k+1) v1 v2
      (VLam x1 env1 b1, VLam x2 env2 b2) ->
          do
            let v = VGen k 
            v1 <- fapp env1 x1 v b1 []
            v2 <- fapp env2 x2 v b2 []
            eqVal (k+1) v1 v2
      (VDef x,VDef y) -> return $ x == y || (error $ "eqVal VDef " ++ show x ++ " " ++ show y)
      (VCon n1,VCon n2) -> return $ n1 == n2 || (error $ "eqVal VCon " ++ show n1 ++ " " ++ show n2)
      _ ->  error $ "eqVal error " ++ show u1 ++ " @@ " ++ show u2

eqVals :: Int -> [Val] -> [Val] -> TypeCheck Bool
eqVals k [] [] = return True
eqVals k (v1:vs1) (v2:vs2) = do eqVal k v1 v2 
                                eqVals k vs1 vs2 
eqVals _ _ _ = return False

-- type checking

checkExpr :: Int -> Env -> Env -> Expr -> Val -> TypeCheck Bool
checkExpr k rho gamma e v = 
    case (e,v) of
      (Lam n e1,VPi x w env t2) -> 
          do
            let v = VGen k
            val <- fapp env x v t2 []
            checkExpr (k+1) (update rho n v) (update gamma n w) e1 val  
      (Pi (TBind n t1) t2,VSet) ->
          do checkType k rho gamma t1 
             val <- eval rho t1
             checkType (k+1) (update rho n (VGen k)) (update gamma n val) t2
      (Fun t1 t2,VSet) -> do checkType k rho gamma t1 
                             checkType k rho gamma t2
      (Succ e2,VSize) -> checkExpr k rho gamma e2 v
      _ -> do v2 <- inferExpr k rho gamma e
              eqVal k v2 v

inferExpr :: Int -> Env -> Env -> Expr -> TypeCheck Val 
inferExpr k rho gamma e = 
    case e of
      Var x -> return $ lookupEnv gamma x
      Set -> return VSet
      Size -> return VSet
      Infty -> return VSize
      App e1 [e2] ->
          do
            v <- inferExpr k rho gamma e1
            case v of
               VPi n w env e3 ->  do checkExpr k rho gamma e2 w 
                                     v2 <- (eval rho e2)
                                     fapp env n v2 e3 [] 
               _ -> error $ "inferExp : expected Pi with expression : " ++ show e1 
 
      App e1 (e2:el) -> inferExpr k rho gamma (App (App e1 [e2]) el)  
      (Def n) -> do sig <- get 
                    case (lookupSig n sig) of
                      (DataSig _ t) -> eval rho t
                      (FunSig _ t _ _) -> eval rho t
                      _ -> error $ "bla" ++ show n
      (Con n) -> do sig <- get
                    case (lookupSig n sig) of
                      (ConSig t) -> eval rho t
                      _ -> error $ "impossible " 
      (Const n) -> do sig <- get 
                      case (lookupSig n sig) of
                        (ConstSig t e) -> eval rho t
      _ -> error $ "cannot infer type " ++ show e

      

checkType :: Int -> Env -> Env -> Expr -> TypeCheck Bool
checkType k rho gamma e = checkExpr k rho gamma e VSet 

-- type check a funtion


checkFun :: Type -> [Clause] -> TypeCheck Bool
checkFun t cll =
    do 
      mapM (checkClause t) cll 
      return True

checkClause :: Type -> Clause -> TypeCheck Bool
checkClause t (Clause (LHS pl) rhs) = 
    do
      v <- eval [] t
      (k,rho,gamma,vt2) <- checkPatterns 0 [] [] v pl
      checkRHS k rho gamma rhs vt2 

checkPatterns :: Int -> Env -> Env -> Val -> [Pattern] -> TypeCheck (Int,Env,Env,Val)
checkPatterns k env1 env2 v pl = 
    case pl of
      [] -> return (k,env1,env2,v)
      (p:pl') -> do (k',env1',env2',v') <- checkPattern k env1 env2 v p 
                    checkPatterns k' env1' env2' v' pl' 
                

checkPattern :: Int -> Env -> Env -> Val -> Pattern -> TypeCheck (Int,Env,Env,Val)
checkPattern k rho gamma v p = 
    case (p,v) of
      (VarP x,VPi y v2 env b) -> do bv <- eval (update env y (VGen k)) b 
                                    return (k+1
                                           ,update rho x (VGen k)
                                           ,update gamma x v2
                                           ,bv
                                           )
      (ConP n pl,VPi y v2 env b) -> do sig <- get 
                                       let (ConSig t) = (lookupSig n sig)
                                       vt <- eval [] t
                                       (k',rho',gamma',v') <- checkPatterns k rho gamma vt pl
                                       pv <- eval rho' (patternToExpr p)
                                       eqVal k' v2 v' 
                                       vb <- eval (update env y pv) b
                                       return (k,rho',gamma',vb)
      (SuccP p2,VPi y v2 env b) -> do (k',rho',gamma',v') <- checkPattern k rho gamma (VPi "" VSize [] Size) p2
                                      pv <- eval rho' (patternToExpr p)
                                      eqVal k' v2 v'
                                      vb <- eval (update env y pv) b
                                      return (k',rho',gamma',vb)
      (WildP,VPi y v2 env b) -> do vb <- eval (update env y (VGen k)) b
                                   return (k+1,rho,gamma,vb) 
      (DotP e,VPi y v2 env b) -> do checkExpr k rho gamma e v2 
                                    v' <- eval rho e
                                    vb <- eval (update env y v') b
                                    return (k+1,rho,gamma,vb)
      (AbsurdP,_) -> return (k,rho,gamma,v)
      _ -> error $ "checkpattern " ++ show p ++ " @ " ++ show v

checkRHS :: Int -> Env -> Env -> RHS -> Val -> TypeCheck Bool
checkRHS k rho gamma rhs v =
    case rhs of 
      (AbsurdRHS) -> return True
      (RHS e) -> checkExpr k rho gamma e v  
      


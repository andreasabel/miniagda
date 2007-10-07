module TypeChecker where

import Abstract
import Value
import Signature


import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Debug.Trace


data TraceError = Err String | TrErr String TraceError

instance Error TraceError where
    noMsg = Err "no message"
    strMsg s = Err s

instance Show TraceError where
    show (Err str) = str
    show (TrErr str err) = str ++ " -> \n" ++ show err 

-- reader monad for local environment (not used right now)
-- state monad for global signature
type TypeCheck a = ReaderT Env (StateT Signature (ErrorT TraceError Identity)) a 


runTypeCheck :: Env -> Signature -> TypeCheck a -> Either TraceError (a,Signature)
runTypeCheck env sig tc = runIdentity (runErrorT (runStateT (runReaderT tc env) sig)) 
 
typeCheck dl = runTypeCheck emptyEnv emptySig (typeCheckDecls dl)
               
typeCheckDecls :: [Declaration] -> TypeCheck ()
typeCheckDecls [] = return ()
typeCheckDecls (d:ds) = do typeCheckDeclaration d
                           typeCheckDecls ds
                           return ()


typeCheckDeclaration :: Declaration -> TypeCheck ()
typeCheckDeclaration (DataDecl n co tel t cs) = 
   (
    do sig <- get
       let dt = (teleToType tel t)
       let params = length tel
       checkType 0 [] [] dt 
       checkTargetSet t
       v <- vclos [] dt
       b <- szType params v
       put (addSig sig n (DataSig params NotSized co v))
       bl <- mapM (typeCheckConstructor n tel t) cs
       case (b && and bl) of
         True -> trace (n ++ " is a sized type ") $ do
                            sig <- get
                            put (addSig sig n (DataSig params Sized co v)) -- update signature 
         False -> return ()
       return () 
   ) `throwTrace` n
typeCheckDeclaration (FunDecl co funs) = typeCheckFuns co funs 
typeCheckDeclaration (ConstDecl (TypeSig n t) e ) = 
   (
    do sig <- get
       checkType 0 [] [] t 
       vt <- vclos [] t
       checkExpr 0 [] [] e vt
       put (addSig sig n (ConstSig vt e))
       return ()
   ) `throwTrace` n
typeCheckFuns :: Co -> [(TypeSig,[Clause])] -> TypeCheck()
typeCheckFuns co funs = do addFunSig funs
                           typeCheckFunSig funs
                           typeCheckFunClause 1 funs
  where
    addFunSig :: [(TypeSig,[Clause])] -> TypeCheck ()
    addFunSig [] = return ()
    addFunSig ((TypeSig n t,cl):rest) = 
        do
          sig <- get 
          vt <- vclos [] t
          ar <- arity co t cl
          put (addSig sig n (FunSig co vt ar cl))
          addFunSig rest
    typeCheckFunSig :: [(TypeSig,[Clause])] -> TypeCheck ()
    typeCheckFunSig [] = return ()
    typeCheckFunSig ((TypeSig n t,cl):rest) = 
       (
        do sig <- get          
           checkType 0 [] [] t
           vt <- eval [] t
           case co of
             Ind -> szCheckIndFun 0 vt
             CoInd -> szCheckCoFun 0 vt
           typeCheckFunSig rest
       ) `throwTrace` ("type of " ++ n)
    typeCheckFunClause :: Int -> [(TypeSig,[Clause])] -> TypeCheck ()
    typeCheckFunClause k [] = return ()
    typeCheckFunClause k ((TypeSig n t,cl):rest) =
       (
        do sig <- get
           checkFun t cl 
           typeCheckFunClause (k+1) rest         
       ) `throwTrace` n         
typeCheckConstructor :: Name -> Telescope -> Type -> Constructor -> TypeCheck Bool
typeCheckConstructor d tel tg (TypeSig n t) = 
   ( 
   do sig <- get 
      let tt = teleToType tel t
      let (args,target) = typeToTele tt
      checkType 0 [] [] tt
      vt <- vclos [] tt
      put (addSig sig n (ConSig vt)) 
      szConstructor d (length tel) vt
   ) `throwTrace` n

-- check that the data type and the parameter arguments fit 
checkTarget :: Name -> Telescope -> Type -> Type -> TypeCheck()
checkTarget d tel dt c = case c of
                           (App (Def n) al) -> if n == d then
                                                   do let pn = length tel
                                                      let params = take pn al
                                                      checkPs tel params -- check parameters
                                                  
                                               else throwErrorMsg "target mismatch"
                           (Def n) -> if n == d && (null tel) then return ()
                                      else throwErrorMsg "target mismatch"
                         
                           _ -> throwErrorMsg "target mismatch"
    where checkPs :: Telescope -> [Expr] -> TypeCheck ()
          checkPs [] [] = return ()
          checkPs ((n,t):tl) ((Var n'):el) = if n == n' then
                                                       checkPs tl el
                                                   else
                                                       throwErrorMsg "target param mismatch"
          checkPs _ _ = throwErrorMsg "target param mismatch"

checkTargetSet :: Type -> TypeCheck()
checkTargetSet t = let (_,target) = typeToTele t 
                   in
                     if target == Set then
                         return ()
                     else
                       throwErrorMsg $ "doesn't target Set " ++ show target


-- for inductive data types, the number of clauses is sufficient
arity :: Co -> Type -> [Clause] -> TypeCheck Int
arity co t cl = do x <- arityClauses cl 
                   y <- case x of
                          Nothing -> throwErrorMsg "error : pattern length mismatch"
                          Just k -> case co of 
                                      Ind -> return $ k
                                      CoInd -> arityType t
                   return y

arityType :: Type -> TypeCheck Int
arityType t = do case t of 
                   (Pi t _ e2) -> do x <- arityType e2
                                     return $ x + 1
                   _ -> return 0

-- check that patterns have the same length, return length
arityClauses :: [Clause] -> TypeCheck (Maybe Int)
arityClauses [] = return $ Just (-1) -- empty clause list  
arityClauses (Clause pl rhs :cl ) = checkLength (length pl) cl 
    where 
      checkLength i [] = return $ Just i
      checkLength i ((Clause pl rhs):xs) = do let x = (length pl) 
                                              case (x == i) of
                                                True ->
                                                    checkLength i xs 
                                                False ->
                                                    return Nothing 
                                        
              
---- Pattern Matching ----

matches :: Pattern -> Val -> Bool
matches p v = case (p,v) of
                (VarP x,_) -> True
                (ConP _ x [],VCon y) -> x == y
                (ConP _ x pl,VApp (VCon y) vl) -> x == y && matchesList pl vl
                (SuccP _,_) -> matchesSucc p v
                (DotP _,_) -> True
                _ -> False
                

matchesSucc :: Pattern -> Val -> Bool
matchesSucc p v = case (p,v) of
                    (_,VInfty) -> True
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
                      (ConP _ x [],VCon y) -> env
                      (ConP _ x pl,VApp (VCon y) vl) -> upPatterns env pl vl
                      (SuccP _,_) -> upSuccPattern env p v
                      (DotP _,_) -> env

upSuccPattern :: Env -> Pattern -> Val -> Env
upSuccPattern env p v = case (p,v) of
                      (SuccP (VarP x),VInfty) -> update env x v
                      (SuccP p2,VInfty) -> upSuccPattern env p2 v 
                      (SuccP (VarP x),VSucc v2) -> update env x v2
                      (SuccP p2,VSucc v2) -> upSuccPattern env p2 v2
                      _ -> error $ "upSuccPattern" ++ show p ++ show v
upPatterns :: Env -> [Pattern] -> [Val] -> Env
upPatterns env [] [] = env
upPatterns env (p:pl) (v:vl) = let env' = upPattern env p v in
                               upPatterns env' pl vl




matchClauses :: [Clause] -> [Val] -> TypeCheck (Maybe Val)
matchClauses cl vl = loop cl
    where loop [] = return Nothing
          loop  (Clause pl rhs : cl2) = 
              do x <- matchClause [] pl rhs vl 
                 case x of
                   Nothing -> loop cl2
                   Just v -> return $ Just v

matchClause :: Env -> [Pattern] -> Expr -> [Val] -> TypeCheck (Maybe Val)
matchClause env [] rhs vl = do v <- eval env rhs
                               v' <- force v
                               v2 <- app v' vl
                               return $ Just v2
matchClause env (p:pl) rhs (v0:vl) =
  do v <- force v0
     let b = matches p v 
     case b of
       True ->
         matchClause (upPattern env p v) pl rhs vl
       False -> return Nothing

patternToExpr :: Pattern -> Expr
patternToExpr p =
    case p of
      VarP n -> Var n
      ConP co n [] -> Con co n
      ConP co n pl -> App (Con co n) (map patternToExpr pl)
      SuccP p -> Succ (patternToExpr p)
      DotP e -> e
     
--- Interpreter

whnf :: Val -> TypeCheck Val
whnf v = 
    --trace ("whnf " ++ show v) $
    case v of
      (VClos _ (Lam _ _)) -> return v
      (VClos _ (Pi _ _ _)) -> return v
      (VClos env e) -> do v' <- eval env e
                          whnf v'
      (VApp v1 vl) -> do v1' <- whnf v1
                         vl' <- mapM whnf vl
                         app v1' vl'
      (VSucc v1) -> do v1' <- whnf v1
                       return $ vsucc v1'
      _ -> return v

app :: Val -> [Val] -> TypeCheck Val
app u v = case (u,v) of
            (VApp u2 v2,_) -> app u2 (v2 ++ v)
            (VClos env (Lam x e),(v:vl))  -> do v' <- eval (update env x v) e
                                                app v' vl
            (VClos env (Pi x _ e),(v:vl)) -> do v' <- eval (update env x v) e
                                                app v' vl
            (VDef n,_) -> appDef n v
            (_,[]) -> return u
            _ -> return $ VApp u v

-- unroll a corecursive object once
force :: Val -> TypeCheck Val
force v@ (VDef n) = 
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity == 0 -> do m <- matchClauses cl []
                                                      case m of
                                                        Nothing -> return v
                                                        Just v2 -> return v2
         _ -> return v 
force v@(VApp (VDef n) vl) =
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= (length vl) -> do m <- matchClauses cl vl
                                                                case m of 
                                                                  Nothing -> return v
                                                                  Just v2 -> return v2
         _ -> return v
force v = return v


canForce :: Val -> TypeCheck Bool
canForce v@ (VDef n) = 
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= 0 -> do m <- matchClauses cl []
                                                      case m of
                                                        Nothing -> return False
                                                        Just v2 -> return True
         _ -> return False 
canForce v@(VApp (VDef n) vl) =
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= (length vl) -> do m <- matchClauses cl vl
                                                                case m of 
                                                                  Nothing -> return False
                                                                  Just v2 -> return True
         _ -> return False
canForce v = return False


appDef :: Name -> [Val] -> TypeCheck Val
appDef n vl = -- trace ("appDef " ++ n ++ show vl) $ 
    do
      sig <- get
      case lookupSig n sig of
        (FunSig Ind   t arity cl) | arity     <= (length vl) -> do m <- matchClauses cl vl 
                                                                   case m of
                                                                     Nothing -> return $ VApp (VDef n) vl
                                                                     Just v2 -> return v2
        (FunSig CoInd t arity cl) | arity + 1 <= (length vl) ->  do m <- matchClauses cl vl
                                                                    case m of
                                                                      Nothing -> return $ VApp (VDef n) vl
                                                                      Just v2 -> return v2
        _ -> case vl of 
               [] -> return $ VDef n  
               _ -> return $ VApp (VDef n) vl   


vsucc :: Val -> Val
vsucc VInfty = VInfty
vsucc x = VSucc x

vclos :: Env -> Expr -> TypeCheck Val
vclos env e = return $ VClos env e


eval :: Env -> Expr -> TypeCheck Val
eval env e =  --trace ("eval " ++ show e) $ 
            case e of
               Set -> return  VSet
               Infty -> return VInfty
               Succ e -> do v <- (eval env e)
                            return $ vsucc v
               Size -> return VSize
               Con _ n -> return $ VCon n
               App e1 e2 -> do v1 <- eval env e1
                               v2 <- mapM (eval env) e2
                               app v1 v2
               Def n -> return $ VDef n
               Const n -> do sig <- get 
                             let (ConstSig _ e) = lookupSig n sig
                             eval [] e
               Var y -> return $ lookupEnv env y
               _ -> return $ VClos env e -- Lam , Pi
                                 
runEval sig e = runTypeCheck emptyEnv sig (eval emptyEnv e)
----


eqVal :: Int -> Val -> Val -> TypeCheck ()
eqVal = eqVal' N


-- force history
data Force = N | L | R -- not yet, left , right  
    deriving Eq

eqVal' :: Force -> Int -> Val -> Val -> TypeCheck ()
eqVal' f k u1 u2 = --trace ("eqVal'" ++ show (u1,u2)) $ 
 do u1 <- whnf u1
    u2 <- whnf u2
    b1 <- canForce u1
    b2 <- canForce u2
    case (b1,b2) of
      (True,False) | f /= R -> -- only unroll one side  
          do u1' <- force u1
             eqVal' L k u1' u2
      (False,True) | f /= L ->
          do u2' <- force u2
             eqVal' R k u1 u2' 
      _ -> case (u1,u2) of
             (VSet,VSet) -> return ()
             (VSize,VSize) -> return ()
             (VInfty,VInfty) -> return ()
             (VSucc v1,VSucc v2) -> eqVal' f k v1 v2
             (VApp v1 w1,VApp v2 w2 ) -> do eqVal' f k v1 v2 
                                            eqVals' f k w1 w2
             (VGen k1,VGen k2) -> if k1 == k2 then return ()
                                  else
                                      throwErrorMsg $ "gen mismatch "  ++ show k1 ++ " " ++ show k2 
             (VClos env1 (Pi x1 a1 b1), VClos env2 (Pi x2 a2 b2)) -> 
                        do a1' <- eval env1 a1
                           a2' <- eval env2 a2
                           eqVal' f k a1' a2'
                           v1 <- eval (update env1 x1 (VGen k)) b1
                           v2 <- eval (update env2 x2 (VGen k)) b2 
                           eqVal' f (k+1) v1 v2
             (VClos env1 (Lam x1 b1), VClos env2 (Lam x2 b2)) ->
                        do v1 <- eval (update env1 x1 (VGen k)) b1
                           v2 <- eval (update env2 x2 (VGen k)) b2
                           eqVal' f (k+1) v1 v2
             (VDef x,VDef y) -> if x == y then return ()
                                else throwErrorMsg $ "eqVal VDef " ++ show x ++ show y 
             (VCon n1,VCon n2) -> if (n1 == n2) then return ()
                                  else throwErrorMsg $ "eqVal VCon " ++ show n1 ++ " " ++ show n2
             _ -> throwErrorMsg $ "eqVal error " ++ show u1 ++ " @@ " ++ show u2


eqVals' :: Force -> Int -> [Val] -> [Val] -> TypeCheck ()
eqVals' f k [] [] = return ()
eqVals' f k (v1:vs1) (v2:vs2) = do eqVal' f k v1 v2 
                                   eqVals' f k vs1 vs2
eqVals' f k vl1 vl2 = throwErrorMsg "mismatch number of arguments"

eqVals :: Int -> [Val ] -> [Val ] -> TypeCheck ()
eqVals = eqVals' N

-- subtyping

leqVal :: Int -> Val -> Val -> TypeCheck ()
leqVal k u1 u2 = -- trace ("leqVal " ++ show (u1,u2) ) $ 
  do
    u1 <- whnf u1
    u2 <- whnf u2
    case (u1,u2) of
      (VSet,VSet) -> return ()
      (VSize,VSize) -> return ()
      (VInfty,VInfty) -> return ()
      (VSucc v1,VSucc v2) -> leqVal k v1 v2
      (v1,VSucc v2) -> leqVal k v1 v2                                            
      (VApp v1 w1,VApp v2 w2 ) -> leqApp k v1 w1 v2 w2
      (VGen k1,VGen k2) -> if k1 == k2 then return () 
                           else throwErrorMsg $ "gen mismatch "  ++ show k1 ++ " " ++ show k2 
      (VClos env1 (Pi x1 a1 b1), VClos env2 (Pi x2 a2 b2)) -> 
          do a1' <- eval env1 a1
             a2' <- eval env2 a2
             leqVal k a2' a1'
             v1 <- eval (update env1 x1 (VGen k)) b1
             v2 <- eval (update env2 x2 (VGen k)) b2
             leqVal (k+1) v1 v2
      (VClos env1 (Lam x1 b1), VClos env2 (Lam x2 b2)) ->
          do v1 <- eval (update env1 x1 (VGen k)) b1
             v2 <- eval (update env2 x2 (VGen k)) b2
             leqVal (k+1) v1 v2
      (VDef x,VDef y) ->  if x == y then return () 
                          else throwErrorMsg $ "leqVal VDef " ++ show x ++ " " ++ show y
      (VCon n1,VCon n2) -> if n1 == n2 then return () 
                           else throwErrorMsg $ "leqVal VCon " ++ show n1 ++ " " ++ show n2
      _ ->  throwErrorMsg $ "leqVal error " ++ show u1 ++ " @@ " ++ show u2



leqApp :: Int -> Val -> [Val] -> Val -> [Val] -> TypeCheck ()
leqApp k v1 w1 v2 w2 = 
    do
      v1 <- whnf v1
      v2 <- whnf v2
      case (v1,v2) of
        (VDef n,VDef m) | n == m -> do sig <- get
                                       case lookupSig n sig of
                                         (DataSig p Sized co _) -> --special treatment of sized data type
                                               do
                                                 let restw1 = (take p w1) ++ (drop (p + 1) w1)
                                                 let restw2 = (take p w2) ++ (drop (p + 1) w1)
                                                 eqVals k restw1 restw2
                                                 case co of -- "substaging" on size index
                                                  Ind ->
                                                      leqSize k (w1 !! p) (w2 !! p)
                                                  CoInd ->
                                                      leqSize k (w1 !! p) (w1 !! p)
                                         _ -> eqVals k w1 w2
        _ -> do leqVal k v1 v2 
                eqVals k w1 w2

-- substaging on size values
leqSize :: Int -> Val -> Val -> TypeCheck ()
leqSize k v1 v2 = --trace ("leqSize " ++ show v1 ++ show v2) $ 
    do v1 <- whnf v1
       v2 <- whnf v2
       case (v1,v2) of
         (_,VInfty) -> return ()
         (VSucc v1,VSucc v2) -> leqSize k v1 v2
         (v1,VSucc v2) -> leqSize k v1 v2
         (VGen v1,VGen v2) | v1 == v2 -> return ()
         _ -> throwErrorMsg $ "leqSize " ++ show v1 ++ " " ++ show v2  
    
leqSizes :: Int -> [Val] -> [Val] -> TypeCheck ()
leqSizes k [] [] = return ()
leqSizes k (v1:vs1) (v2:vs2) = do leqSize k v1 v2 
                                  leqSizes k vs1 vs2
leqSizes k vl1 vl2 = throwErrorMsg "mismatch number of arguments"
 

-- type checking

checkExpr :: Int -> Env -> Env -> Expr -> TVal -> TypeCheck ()
checkExpr k rho gamma e v = --trace ("checkExpr " ++ show e) $
  do
    v <- whnf v 
    case (e,v) of
      (Lam n e1,VClos env (Pi x w t2)) -> 
          do
            w' <- vclos env w
            val <- eval (update env x (VGen k)) t2
            checkExpr (k+1) (update rho n (VGen k)) (update gamma n w') e1 val  
      (Pi n t1 t2,VSet) ->
          do checkType k rho gamma t1
             val <- vclos rho t1 
             checkType (k+1) (update rho n (VGen k)) (update gamma n val) t2
      (Succ e2,VSize) -> checkExpr k rho gamma e2 VSize
      _ -> do v2 <- inferExpr k rho gamma e
              leqVal k v2 v

inferExpr :: Int -> Env -> Env -> Expr -> TypeCheck TVal 
inferExpr k rho gamma e = 
    --trace ("inferExpr " ++ show e) $
    case e of
      Var x -> return $ lookupEnv gamma x
      Set -> return VSet
      Size -> return VSet
      Infty -> return VSize
      App e1 [e2] ->
          do
            v <- inferExpr k rho gamma e1
            v <- whnf v
            case v of
               VClos env (Pi n t1 t2) ->  do tv1' <- vclos env t1 
                                             checkExpr k rho gamma e2 tv1' 
                                             v2 <- vclos rho e2
                                             eval (update env n v2) t2  
               _ -> throwErrorMsg $ "inferExpr : expected Pi with expression : " ++ show e1 ++ "," ++ show v
 
      App e1 (e2:el) -> inferExpr k rho gamma (App (App e1 [e2]) el)  
      (Def n) -> do sig <- get 
                    case (lookupSig n sig) of
                      (DataSig _ _ _ tv) -> return tv
                      (FunSig _ tv _ _) -> return tv
      (Con _ n) -> do sig <- get
                      case (lookupSig n sig) of
                        (ConSig tv) -> return tv
      (Const n) -> do sig <- get 
                      case (lookupSig n sig) of
                        (ConstSig tv _) -> return tv
      _ -> throwErrorMsg $ "cannot infer type " ++ show e

      

checkType :: Int -> Env -> Env -> Expr -> TypeCheck ()
checkType k rho gamma e = checkExpr k rho gamma e VSet 

-- type check a funtion

checkFun = checkFun' 1 

checkFun' :: Int -> Type -> [Clause] -> TypeCheck ()
checkFun' k t [] = return ()
checkFun' k t (c:cl) = do checkClause k t c
                          checkFun' (k + 1) t cl

checkClause :: Int -> Type -> Clause -> TypeCheck ()
checkClause k t (Clause pl rhs) = 
    (
    do
      v <- vclos [] t
      (k,flex,ins,rho,gamma,vt) <- checkPatterns 0 [] [] [] [] v pl
      gamma <- substEnv ins gamma
      mapM (checkDot k rho gamma ins) flex
      checkRHS k rho gamma rhs vt
    ) `throwTrace` ("clause " ++ show k)
checkPatterns :: Int -> [(Int,Expr)] -> [(Int,Val)] -> Env -> Env -> TVal -> [Pattern] -> TypeCheck (Int,[(Int,Expr)],[(Int,Val)],Env,Env,TVal)
checkPatterns k flex ins rho gamma v pl = 
    case pl of
      [] -> return (k,flex,ins,rho,gamma,v)
      (p:pl') -> do (k',flex',ins',rho',gamma',v') <- checkPattern k flex ins rho gamma v p 
                    checkPatterns k' flex' ins' rho' gamma' v' pl' 
       
{- 
checkPattern k flex subst rho gamma v p = (k', flex', subst', rho', gamma', v')

Input : 
  k     : next free generic value
  flex  : list of pairs (flexible variable, its dot pattern)
  subst : list of pairs (flexible variable, its valuation)
  rho   : binding of variables to values
  gamma : binding of variables to their types
  v     : type of the expression \ p -> t
  p     : the pattern to check

Output
  v'    : type of t
-}

checkPattern :: Int -> [(Int,Expr)] -> [(Int,Val)] -> Env -> Env -> TVal -> Pattern -> TypeCheck (Int,[(Int,Expr)],[(Int,Val)],Env,Env,TVal)
checkPattern k flex ins rho gamma v p = -- trace ("cp " ++ show k ++ " " ++ show flex ++ " " ++ show rho ++ " " ++ show gamma ++ " " ++ show v ++ " " ++ show p) $ 
 (
 do 
  v <- whnf v
  v <- force v 
  case v of
    VClos env (Pi y a b) -> do
        av <- eval env a
        av <- whnf av
        case p of
          VarP x -> do let gk = VGen k 
                       bv <- eval (update env y gk) b 
                       return (k+1
                              ,flex
                              ,ins
                              ,update rho x gk
                              ,update gamma x av
                              ,bv
                              )
          ConP co n pl -> do 
                 sig <-  get 
                 let (ConSig tv) = (lookupSig n sig)
                 tv <- whnf tv
                 (k',flex',ins',rho',gamma',v') <- checkPatterns k flex ins rho gamma tv pl
                 subst <- inst k' (fst $ unzip flex') v' av
                 pv <- vclos rho' (patternToExpr p)
                 vb <- eval (update env y pv) b
                 vb <- substVal subst vb
                 gamma' <- substEnv subst gamma'
                 return (k',flex',ins' ++ subst,rho',gamma',vb)
          SuccP p2 -> do (k',flex',ins',rho',gamma',v') <- checkPattern 
                                                             k flex ins rho gamma (VClos [] (Pi "" Size Size)) p2
                         eqVal k' av v'
                         pv <- vclos rho' (patternToExpr p)
                         vb <- eval (update env y pv) b
                         return (k',flex',ins',rho',gamma',vb)
          DotP e -> do vb <- eval (update env y (VGen k)) b
                       return (k+1
                              ,(k,e):flex
                              ,ins
                              ,rho
                              ,gamma
                              ,vb)
          _ -> error $ "checkpattern " ++ show p ++ " @ " ++ show v
    _ -> error $ "checkpattern " ++ show p ++ " @ " ++ show v
  ) `throwTrace` ("pattern " ++ show p)

checkDot :: Int -> Env -> Env -> [(Int,Val)] -> (Int,Expr) -> TypeCheck()
checkDot i rho gamma ins (k,e) = -- trace ("checking dot pattern " ++ show e) $ 
                                         (
                                         case (lookup k ins) of
                                           Nothing -> throwErrorMsg $ "not instantiated "
                                           Just v -> do v' <- eval rho e
                                                        v'' <- substVal ins v
                                                        eqVal i v' v''
                                         ) `throwTrace` ("dot pattern " ++ show e)            
checkRHS :: Int -> Env -> Env -> Expr -> TVal -> TypeCheck ()
checkRHS k rho gamma rhs v = -- trace ("checking rhs") $ 
                     (
                      checkExpr k rho gamma rhs v  
                     ) `throwTrace` "right hand side"

inst :: Int -> [Int] -> Val -> Val -> TypeCheck [(Int,Val)]
inst m flex v1 v2 = do
  v1 <- whnf v1
  v2 <- whnf v2
  case (v1,v2) of
    (VGen k,_) | elem k flex -> return [(k,v2)]
    (_,VGen k) | elem k flex -> return [(k,v1)]
    (VApp (VDef d1) vl1,VApp (VDef d2) vl2) | d1 == d2 -> instList m flex vl1 vl2
    (VApp (VCon c1) vl1,VApp (VCon c2) vl2) | c1 == c2 -> instList m flex vl1 vl2
    (VSucc v1',VSucc v2') -> inst m flex v1' v2'
    (VSucc v, VInfty) -> inst m flex v VInfty
    _ -> do eqVal m v1 v2
            return []
   
instList :: Int -> [Int] -> [Val] -> [Val] -> TypeCheck [(Int,Val)]
instList m flex [] [] = return []
instList m flex (v1:vl1) (v2:vl2) = do map <- inst m flex v1 v2
                                       vl1' <- mapM (substVal map) vl1
                                       vl2' <- mapM (substVal map) vl2
                                       map' <- instList m flex vl1 vl2
                                       return $ map ++ map'


-- substitute generic variable in value
substVal :: [(Int,Val)] -> Val -> TypeCheck Val
substVal map v = -- trace ("substval " ++ show v) $ 
  do
    case v of
      (VGen k) -> case lookup k map of
                   Nothing -> return v
                   Just v' -> return v'
      (VApp v1 vl) -> do v1' <- substVal map v1
                         vl <- mapM (substVal map) vl
                         return $ VApp v1' vl
      (VSucc v1) -> do v1' <- substVal map v1
                       return $ VSucc v1'
      (VClos env e) -> do env' <- substEnv map env
                          return $ VClos env' e
      _ -> return v


-- substitute in environment
substEnv :: [(Int,Val)] -> Env -> TypeCheck Env
substEnv map [] = return []
substEnv map ((x,v):env) = do v' <- substVal map v
                              env' <- substEnv map env
                              return $ (x,v'):env'


--------------
---- Size checking
--------------

-- check wether the data type is sized type


-- check data declaration type 
-- parameters : number of params, type 
szType :: Int -> TVal -> TypeCheck Bool
szType p tv = do (tv',k) <- doParams p 0 tv
                 case tv' of
                   (VClos env (Pi x a b)) -> do
                       av <- eval env a
                       av <- whnf av
                       case av of
                         VSize -> return True
                         _ -> return False
                   _ -> return False
                 
-- check data constructors
szConstructor :: Name -> Int -> TVal -> TypeCheck Bool
szConstructor n p tv = do (tv',k) <- doParams p 0 tv
                          case tv' of
                            VClos env (Pi x a b) -> do
                                     av <- eval env a
                                     av <- whnf av
                                     case av of
                                       VSize -> do bv <- eval (update env x (VGen k)) b 
                                                   szSizeVarUsage n (k+1) p k bv
                                       _ -> return False
                            _ -> return False

szSizeVarUsage :: Name -> Int -> Int -> Int -> TVal -> TypeCheck Bool 
szSizeVarUsage n k p i tv = do tv' <- whnf tv
                               case tv' of
                                 VClos env (Pi x a b) -> do
                                          av <- eval env a
                                          av <- whnf av
                                          b1 <- szSizeVarDataArgs n k p i av
                                          bv <- eval (update env x (VGen k)) b
                                          b2 <- szSizeVarUsage n (k+1) p i bv
                                          return $ b1 && b2
                                 _ -> szSizeVarTarget k p i tv


-- check that Target is of form D ... (Succ i) ... 
szSizeVarTarget :: Int -> Int -> Int -> TVal -> TypeCheck Bool
szSizeVarTarget k p i tv = do
                             (VApp v1 vl) <- whnf tv
                             case (vl !! p) of
                               (VSucc (VGen i')) -> case i' == i of
                                                      True -> do let rargs = take p vl ++ drop (p+1) vl
                                                                 bl <- mapM (szSizeVarNotUsed k i) rargs
                                                                 return $ and bl
                                                      False -> return False
                               _ -> return False


-- check that rec. arguments are of form D ... i .... 
szSizeVarDataArgs :: Name -> Int -> Int -> Int -> TVal -> TypeCheck Bool
szSizeVarDataArgs n k p i tv = do
  tv <- whnf tv
  case tv of
    VGen i' -> return $ i' /= i
    VSucc tv' -> szSizeVarDataArgs n k p i tv'
    VApp (VDef m) vl | n == m -> case (vl !! p) of
                                   (VGen i') -> case i' == i of
                                                      True -> do let rargs = take p vl ++ drop (p+1) vl
                                                                 bl <- mapM (szSizeVarNotUsed k i) rargs
                                                                 return $ and bl
                                                      False -> return False
                                   _ -> return False
    VApp v1 vl -> do b <- szSizeVarNotUsed k i v1
                     bl <- mapM (szSizeVarDataArgs n k p i) vl
                     return $ b && and bl
    VClos env (Pi x a b) -> do va <-  eval env a
                               va <- whnf va
                               let k' = k + 1
                               vb <- eval (update env x (VGen k)) b
                               b1 <- szSizeVarDataArgs n k' p i va
                               b2 <- szSizeVarDataArgs n k' p i vb
                               return $ b1 && b2
    _ -> return True


szSizeVarNotUsed :: Int -> Int -> TVal -> TypeCheck Bool
szSizeVarNotUsed k i tv = do
  tv <- whnf tv
  case tv of
    VGen i' -> return $ i' /= i
    VSucc tv' -> szSizeVarNotUsed k i tv'
    VApp v1 vl -> do b <- szSizeVarNotUsed k i v1
                     bl <- mapM (szSizeVarNotUsed k i) vl
                     return $ b && and bl
    VClos env (Pi x a b) -> do va <-  eval env a
                               va <- whnf va
                               let k' = k + 1
                               vb <- eval (update env x (VGen k)) b
                               b1 <- szSizeVarNotUsed k' i va
                               b2 <- szSizeVarNotUsed k' i vb
                               return $ b1 && b2
    _ -> return True

doParams :: Int -> Int -> TVal -> TypeCheck (TVal,Int)
doParams 0 k tv = do tv' <- whnf tv
                     return (tv',k)
doParams p k tv = do
  (VClos env (Pi x a b)) <- whnf tv
  av <- eval env a
  bv <- eval (update env x (VGen k)) b
  doParams (p - 1 ) (k + 1) bv

--------------------------------------
-- check if sizes used correctly in funs and cofuns


-- for inductive funs, every size has to be used at least once in a inductive argument
szCheckIndFun :: Int -> TVal -> TypeCheck ()
szCheckIndFun k tv = 
  do tv <- whnf tv
     case tv of
       VClos env (Pi x a b) -> do av <- eval env a
                                  bv <- eval (update env x (VGen k)) b
                                  let k' = k + 1 
                                  _ <- case av of
                                         VSize -> do g <- szCheckIndFunSize k' k 0 bv
                                                     case g of
                                                       0 -> throwErrorMsg $ "Size " ++ x ++ " not used in sized inductive type arg"
                                                       _ -> return ()
                                         _ -> return ()
                                  szCheckIndFun k' bv
       _ -> return ()

szCheckIndFunSize :: Int -> Int -> Int -> TVal -> TypeCheck Int
szCheckIndFunSize k i good tv = do
  do tv <- whnf tv
     case tv of 
       VClos env (Pi x a b) ->  do av <- eval env a
                                   av <- whnf av
                                   bv <- eval (update env x (VGen k)) b
                                   let k' = k + 1 
                                   case av of
                                     (VApp (VDef n) vl) -> 
                                         do sig <- get
                                            case (lookupSig n sig) of
                                              (DataSig p Sized Ind _) -> 
                                                  do b <- szSizeVarArgs k' p i vl 
                                                     case b of
                                                       False -> szCheckIndFunSize k' i good bv
                                                       True -> szCheckIndFunSize k' i (good + 1) bv
                                              _ -> do b1 <- szSizeVarNotUsed k' i av 
                                                      case b1 of
                                                        True -> szCheckIndFunSize k' i good bv
                                                        False -> throwErrorMsg $ "size var unallawed use " ++ show av
                                     _ -> do b1 <- szSizeVarNotUsed k' i av
                                             case b1 of
                                               True -> szCheckIndFunSize k' i good bv
                                               False -> throwErrorMsg $ "size var unallawed use " ++ show av
       _ -> return good

-- check that arguments are of the form  ... i ..... 
szSizeVarArgs :: Int -> Int -> Int -> [TVal] -> TypeCheck Bool
szSizeVarArgs k p i vl = do case p <= length vl of
                              True ->
                                  case (vl !! p) of
                                    (VGen i') | i == i' -> do let rargs = take p vl ++ drop (p+1) vl
                                                              bl <- mapM (szSizeVarNotUsed k i) rargs
                                                              return $ and bl
                                    (VSucc _) -> throwErrorMsg $ "size succ used" 
                                    _ -> return False
                              False -> return False


-- for a cofun, the size has to be used in a coinductive result type
szCheckCoFun :: Int -> TVal -> TypeCheck ()
szCheckCoFun k tv = 
  do tv <- whnf tv
     case tv of
       VClos env (Pi x a b) -> do av <- eval env a
                                  bv <- eval (update env x (VGen k)) b
                                  let k' = k + 1 
                                  _ <- case av of
                                         VSize -> do b <- szCheckCoFunSize k' k bv
                                                     case b of
                                                       False -> throwErrorMsg $ "Size " ++ x ++ " not used in sized coinductive result type"
                                                       True -> return ()
                                         _ -> return ()
                                  szCheckCoFun k' bv
       -- result 
       (VApp (VDef n) vl) -> 
          do sig <- get
             case (lookupSig n sig) of
               (DataSig _ _ CoInd _) -> 
                   return ()
               _ -> throwErrorMsg "cofun doesn't target coinductive type"
       (VDef n)  -> 
          do sig <- get
             case (lookupSig n sig) of
               (DataSig _ _ CoInd _) -> 
                   return ()
               _ -> throwErrorMsg "cofun doesn't target coinductive type"
       _ -> throwErrorMsg "cofun doesn't target coinductive type"


szCheckCoFunSize :: Int -> Int -> TVal -> TypeCheck Bool
szCheckCoFunSize k i tv = -- trace ("szco " ++ show tv) $
  do tv <- whnf tv
     case tv of 
       VClos env (Pi x a b) ->  do bv <- eval (update env x (VGen k)) b
                                   let k' = k + 1 
                                   szCheckCoFunSize k' i bv
       -- result
       (VApp (VDef n) vl) -> 
          do sig <- get
             case (lookupSig n sig) of
               (DataSig p Sized CoInd _) -> 
                   szSizeVarArgs k p i vl 
               _ -> return False
       _ -> return False

throwErrorMsg m = throwError (Err m) 


throwTrace x n = x `catchError` ( \e -> throwError $ TrErr n e) 
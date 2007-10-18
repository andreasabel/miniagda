module TypeChecker where

import Abstract
import Value
import SPos

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Debug.Trace


runTypeCheck :: Env -> Signature -> TypeCheck a -> Either TraceError (a,Signature)
runTypeCheck env sig tc = runIdentity (runErrorT (runStateT (runReaderT tc env) sig)) 
 
typeCheck dl = runTypeCheck emptyEnv emptySig (typeCheckDecls dl)
               
typeCheckDecls :: [Declaration] -> TypeCheck ()
typeCheckDecls [] = return ()
typeCheckDecls (d:ds) = do typeCheckDeclaration d
                           typeCheckDecls ds
                           return ()

typeCheckDeclaration :: Declaration -> TypeCheck ()
typeCheckDeclaration (DataDecl n co pos tel t cs) = 
   (
    do sig <- get
       let dt = (teleToType tel t)
       let params = length tel
       checkType 0 [] [] dt 
       checkTargetSet t
       v <- vclos [] dt
       b <- szType params v
       put (addSig sig n (DataSig params pos NotSized co v))
       bl <- mapM (typeCheckConstructor n pos tel t) cs
       case (b && and bl) of
         True -> trace (n ++ " is a sized type ") $ do
                            sig <- get
                            put (addSig sig n (DataSig params pos Sized co v)) -- update signature 
         False -> return ()
   ) `throwTrace` n

typeCheckDeclaration (ConstDecl _ (TypeSig n t) e ) = 
   (
    do sig <- get
       checkType 0 [] [] t 
       vt <- vclos [] t
       checkExpr 0 [] [] e vt
       put (addSig sig n (ConstSig vt e))
   ) `throwTrace` n
typeCheckDeclaration (NoRecDecl (TypeSig n t) cl) = 
  (
    do
      checkType 0 [] [] t
      checkFun t cl
      sig <- get
      vt <- vclos [] t
      ar <- arity Ind t cl
      put (addSig sig n (FunSig Ind vt ar cl) )
  ) `throwTrace` n
typeCheckDeclaration (FunDecl co funs) = typeCheckFuns co funs

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
        do checkType 0 [] [] t
           vt <- vclos [] t
           case co of
             Ind -> szCheckIndFun 0 vt
             CoInd -> szCheckCoFun 0 vt
           typeCheckFunSig rest
       ) `throwTrace` ("type of " ++ n)
    typeCheckFunClause :: Int -> [(TypeSig,[Clause])] -> TypeCheck ()
    typeCheckFunClause k [] = return ()
    typeCheckFunClause k ((TypeSig n t,cl):rest) =
       (do checkFun t cl 
           typeCheckFunClause (k+1) rest         
       ) `throwTrace` n         
typeCheckConstructor :: Name -> [Pos] -> Telescope -> Type -> Constructor -> TypeCheck Bool
typeCheckConstructor d pos tel tg (TypeSig n t) = 
   ( 
   do sig <- get 
      let tt = teleToType tel t
      let (args,target) = typeToTele tt
      checkType 0 [] [] tt
      vt <- vclos [] tt
      sposConstructor d 0 pos vt
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
    
                               
----


eqVal :: Int -> Val -> Val -> TypeCheck ()
eqVal = eqVal' N


-- force history
data Force = N | L | R -- not yet, left , right  
    deriving (Eq,Show)

switch :: Force -> Force
switch L = R
switch R = L
switch N = N

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
                        do a1' <- vclos env1 a1
                           a2' <- vclos env2 a2
                           eqVal' f k a1' a2'
                           v1 <- vclos (update env1 x1 (VGen k)) b1
                           v2 <- vclos (update env2 x2 (VGen k)) b2 
                           eqVal' f (k+1) v1 v2
             (VClos env1 (Lam x1 b1), VClos env2 (Lam x2 b2)) ->
                        do v1 <- vclos (update env1 x1 (VGen k)) b1
                           v2 <- vclos (update env2 x2 (VGen k)) b2
                           eqVal' f (k+1) v1 v2
             (VDef x,VDef y) -> if x == y then return ()
                                else throwErrorMsg $ "eqVal VDef " ++ show x ++ show y 
             (VCon _ n1,VCon _ n2) -> if (n1 == n2) then return ()
                                      else throwErrorMsg $ "eqVal VCon " ++ show n1 ++ " " ++ show n2
             _ -> throwErrorMsg $ "eqVal error " ++ show f ++ " " ++ show u1 ++ " @@ " ++ show u2


eqVals' :: Force -> Int -> [Val] -> [Val] -> TypeCheck ()
eqVals' f k [] [] = return ()
eqVals' f k (v1:vs1) (v2:vs2) = do eqVal' f k v1 v2 
                                   eqVals' f k vs1 vs2
eqVals' f k vl1 vl2 = throwErrorMsg "mismatch number of arguments"

eqVals :: Int -> [Val ] -> [Val ] -> TypeCheck ()
eqVals = eqVals' N

-- subtyping

leqVal :: Int -> Val -> Val -> TypeCheck ()
leqVal = leqVal' N

leqVal' :: Force -> Int -> Val -> Val -> TypeCheck ()
leqVal' f k u1 u2 = -- trace ("leqVal' " ++ show (u1,u2) ) $ 
  do
    u1 <- whnf u1
    u2 <- whnf u2
    b1 <- canForce u1
    b2 <- canForce u2
    case (b1,b2) of
      (True,False) | f /= R -> -- only unroll one side  
          do u1' <- force u1
             leqVal' L k u1' u2
      (False,True) | f /= L ->
          do u2' <- force u2
             leqVal' R k u1 u2'
      _ ->  case (u1,u2) of
              (VSet,VSet) -> return ()
              (VSize,VSize) -> return ()
              (VInfty,VInfty) -> return ()
              (VSucc v1,VSucc v2) -> leqVal' f k v1 v2
              (VApp v1 w1,VApp v2 w2 ) -> leqApp f k v1 w1 v2 w2
              (VGen k1,VGen k2) -> if k1 == k2 then return () 
                                   else throwErrorMsg $ "gen mismatch "  ++ show k1 ++ " " ++ show k2 
              (VClos env1 (Pi x1 a1 b1), VClos env2 (Pi x2 a2 b2)) -> 
                 do a1' <- vclos env1 a1
                    a2' <- vclos env2 a2
                    leqVal' (switch f) k a2' a1'
                    v1 <- vclos (update env1 x1 (VGen k)) b1
                    v2 <- vclos (update env2 x2 (VGen k)) b2
                    leqVal' f (k+1) v1 v2
              (VClos env1 (Lam x1 b1), VClos env2 (Lam x2 b2)) ->
                  do v1 <- vclos (update env1 x1 (VGen k)) b1
                     v2 <- vclos (update env2 x2 (VGen k)) b2
                     leqVal' f (k+1) v1 v2
              (VDef x,VDef y) ->  if x == y then return () 
                                  else throwErrorMsg $ "leqVal VDef " ++ show x ++ " " ++ show y
              (VCon _ n1,VCon _ n2) -> if n1 == n2 then return () 
                                       else throwErrorMsg $ "leqVal VCon " ++ show n1 ++ " " ++ show n2
              _ ->  throwErrorMsg $ "leqVal error " ++ show u1 ++ " @@ " ++ show u2

leqVals' :: Force -> Int -> [Val] -> [Val] -> TypeCheck ()
leqVals' f k [] [] = return ()
leqVals' f k (v1:vs1) (v2:vs2) = do leqVal' f k v1 v2 
                                    leqVals' f k vs1 vs2
leqVals' f k vl1 vl2 = throwErrorMsg "mismatch number of arguments"


leqApp :: Force -> Int -> Val -> [Val] -> Val -> [Val] -> TypeCheck ()
leqApp f k v1 w1 v2 w2 =  --trace ("leqApp " ++ show v1 ++ show w1 ++ "<=" ++ show v2 ++ show w2) $ 
    do
      case (v1,v2) of
        (VDef n,VDef m) | n == m -> do sig <- get
                                       case lookupSig n sig of
                                         (DataSig p pos s co _) -> -- data type 
                                               do
                                                 let params1 = (take p w1)
                                                 let (pparams1,nparams1) = posArgs params1 pos
                                                 let params2 = (take p w2)
                                                 let (pparams2,nparams2) = posArgs params2 pos
                                                 leqVals' f k pparams1 pparams2 -- ok because parameters strictly pos
                                                 let indices1 = drop (p + 1) w1
                                                 let indices2 = drop (p + 1) w2
                                                 eqVals' f k (nparams1 ++ indices1) (nparams2 ++ indices2) 
                                                 -- now one index (that may be a size) remains to be checked
                                                 case (length w2 > p && length w2 > p) of
                                                   True ->
                                                       case s of
                                                         Sized -> -- sized data type 
                                                              case co of -- "substaging" on size index
                                                                Ind ->
                                                                    leqSize k (w1 !! p) (w2 !! p)
                                                                CoInd ->
                                                                    leqSize k (w2 !! p) (w1 !! p)
                                                         NotSized -> 
                                                                    eqVal k (w1 !! p) (w2 !!p)
                                                   False -> return ()
                                                
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
checkExpr k rho gamma e v = --trace ("checkExpr " ++ show e ++ " " ++ show v) $
  do
    v <- whnf v 
    case (e,v) of
      (Lam n e1,VClos env (Pi x w t2)) -> 
          do
            w' <- vclos env w
            val <- vclos (update env x (VGen k)) t2
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
                                             vclos (update env n v2) t2  
               _ -> throwErrorMsg $ "inferExpr : expected Pi with expression : " ++ show e1 ++ "," ++ show v
 
      App e1 (e2:el) -> inferExpr k rho gamma (App (App e1 [e2]) el)  
      (Def n) -> do sig <- get 
                    case (lookupSig n sig) of
                      (DataSig _ _ _ _ tv) -> return tv
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

type Substitution = [(Int,Val)]


checkPatterns :: Int -> [(Int,Expr)] -> Substitution -> Env -> Env -> TVal -> [Pattern] -> TypeCheck (Int,[(Int,Expr)],Substitution,Env,Env,TVal)
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

checkPattern :: Int -> [(Int,Expr)] -> Substitution -> Env -> Env -> TVal -> Pattern -> TypeCheck (Int,[(Int,Expr)],Substitution,Env,Env,TVal)
checkPattern k flex ins rho gamma v p = -- trace ("cp " ++ show k ++ " " ++ show flex ++ " " ++ show rho ++ " " ++ show gamma ++ " " ++ show v ++ " " ++ show p) $ 
 (
 do 
  v <- whnf v
  case v of
    VClos env (Pi y a b) -> do
        av <- vclos env a
        av <- whnf av
        case p of
          VarP x -> do let gk = VGen k 
                       bv <- vclos (update env y gk) b 
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
                 subst <- instantiate k' (fst $ unzip flex') v' av
                 pv <- patternToValue k p
                 vb <- vclos (update env y pv) b
                 vb <- substVal subst vb
                 gamma' <- substEnv subst gamma'
                 ins'' <- flattenSubst (ins' ++ subst)
                 return (k',flex',ins'',rho',gamma',vb)
          SuccP p2 -> do (k',flex',ins',rho',gamma',v') <- checkPattern 
                                                             k flex ins rho gamma (VClos [] (Pi "" Size Size)) p2
                         eqVal k' av v'
                         pv <- patternToValue k p
                         vb <- vclos (update env y pv) b
                         return (k',flex',ins',rho',gamma',vb)
          DotP e -> do vb <- vclos (update env y (VGen k)) b
                       return (k+1
                              ,(k,e):flex
                              ,ins
                              ,rho
                              ,gamma
                              ,vb)
          _ -> throwErrorMsg $ "checkpattern " ++ show p 
    _ -> throwErrorMsg $ "checkpattern " ++ show p 
  ) `throwTrace` ("pattern " ++ show p)


patternToValue :: Int -> Pattern -> TypeCheck Val
patternToValue k p = do (v,_) <- patternToVal k p
                        return $ v

-- turn a pattern into a value
-- variables and dot patterns are getting the ascending generic values from left to right as during checkpattern
patternToVal :: Int -> Pattern -> TypeCheck (Val,Int)
patternToVal k p = 
    case p of
      VarP n -> return $ (VGen k,k+1)
      ConP co n [] -> return $ (VCon co n,k)
      ConP co n pl -> do
             (vl,k') <- patternsToVal k pl
             return $ (VApp (VCon co n) vl,k')
      SuccP p -> do (v,k') <- patternToVal k p
                    return $ (VSucc v,k')   
      DotP e ->  return $ (VGen k,k+1)

patternsToVal :: Int -> [Pattern] -> TypeCheck ([Val],Int)
patternsToVal k [] = return $ ([],k)
patternsToVal k (p:pl) = do (v,k') <- patternToVal k p
                            (vl,k'') <- patternsToVal k' pl
                            return (v:vl,k'')


checkDot :: Int -> Env -> Env -> Substitution -> (Int,Expr) -> TypeCheck()
checkDot i rho gamma subst (k,e) =  --trace ("checking dot pattern " ++ show e) $ 
                                         (
                                         case (lookup k subst) of
                                           Nothing -> throwErrorMsg $ "not instantiated "
                                           Just v -> do v' <-  vclos rho e
                                                        eqVal i v v'
                                         ) `throwTrace` ("dot pattern " ++ show e )            
checkRHS :: Int -> Env -> Env -> Expr -> TVal -> TypeCheck ()
checkRHS k rho gamma rhs v =  --trace ("checking rhs") $ 
                     (
                      checkExpr k rho gamma rhs v  
                     ) `throwTrace` "right hand side"


-- instantiate the parameters and family indices
-- this yields a substitutin for the flexible variables 
instantiate :: Int -> [Int] -> Val -> Val -> TypeCheck Substitution
instantiate m flex v1 v2 = do subst <- inst m flex v1 v2
                              flattenSubst subst

inst :: Int -> [Int] -> Val -> Val -> TypeCheck Substitution
inst m flex v1 v2 =  do
 v1 <- whnf v1
 v2 <- whnf v2
 case (v1,v2) of
    (VGen k,_) | elem k flex -> return [(k,v2)]
    (_,VGen k) | elem k flex -> return [(k,v1)]
    (VApp (VDef d1) vl1,VApp (VDef d2) vl2) | d1 == d2 -> instList m flex vl1 vl2
    (VApp (VCon _ c1) vl1,VApp (VCon _ c2) vl2) | c1 == c2 -> instList m flex vl1 vl2
    (VSucc v1',VSucc v2') -> inst m flex v1' v2'
    (VSucc v, VInfty) -> inst m flex v VInfty
    _ -> do eqVal m v1 v2
            return []
   
instList :: Int -> [Int] -> [Val] -> [Val] -> TypeCheck Substitution
instList m flex [] [] = return []
instList m flex a@(v1:vl1) b@(v2:vl2) = do map <- inst m flex v1 v2
                                           vl1' <- mapM (substVal map) vl1
                                           vl2' <- mapM (substVal map) vl2
                                           map' <- instList m flex vl1' vl2'
                                           return $ map ++ map'



-- substitute generic variable in value
substVal :: Substitution -> Val -> TypeCheck Val
substVal map v =  
  do
    case v of
      (VGen k) -> case lookup k map of
                   Nothing -> return v
                   Just v' -> return v'
      (VApp v1 vl) -> do v1' <- substVal map v1
                         vl' <- mapM (substVal map) vl
                         return $ VApp v1' vl'
      (VSucc v1) -> do v1' <- substVal map v1
                       vsucc v1'
      (VClos env e) -> do env' <- substEnv map env
                          return $ VClos env' e
      _ -> return v

-- substitute in environment
substEnv :: Substitution -> Env -> TypeCheck Env
substEnv map [] = return []
substEnv map ((x,v):env) = do v' <- substVal map v
                              env' <- substEnv map env
                              return $ (x,v'):env'




-- ensure that no generic value of the codomain appears in a target
flattenSubst :: Substitution -> TypeCheck Substitution
flattenSubst subst = flatten subst 0

flatten subst n =  
  if (n == length subst) then
      return subst 
  else
      do
        let f = map fst subst
        let s = map snd subst
        let bla = subst !! n
        s' <- mapM (substVal ([subst !! n]) ) s
        let subst' = zip f s'
        flatten subst' (n + 1)
                   
--------
---- Size checking
--------------

-- check wether the data type is sized type


-- check data declaration type 
-- parameters : number of params, type 
szType :: Int -> TVal -> TypeCheck Bool
szType p tv = do (tv',k) <- doParams p 0 tv
                 case tv' of
                   (VClos env (Pi x a b)) -> do
                       av <- vclos env a
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
                                     av <- vclos env a
                                     av <- whnf av
                                     case av of
                                       VSize -> do bv <- vclos (update env x (VGen k)) b 
                                                   szSizeVarUsage n (k+1) p k bv
                                       _ -> return False
                            _ -> return False

szSizeVarUsage :: Name -> Int -> Int -> Int -> TVal -> TypeCheck Bool 
szSizeVarUsage n k p i tv = do tv' <- whnf tv
                               case tv' of
                                 VClos env (Pi x a b) -> do
                                          av <- vclos env a
                                          av <- whnf av
                                          b1 <- szSizeVarDataArgs n k p i av
                                          bv <- vclos (update env x (VGen k)) b
                                          b2 <- szSizeVarUsage n (k+1) p i bv
                                          return $ b1 && b2
                                 _ -> szSizeVarTarget k p i tv


-- check that Target is of form D ... (Succ i) ... 
szSizeVarTarget :: Int -> Int -> Int -> TVal -> TypeCheck Bool
szSizeVarTarget k p i tv = do
                             (VApp v1 vl) <- whnf tv
                             v0 <- whnf (vl !! p)
                             case v0 of
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
    VApp (VDef m) vl | n == m -> do 
                                 v0 <- whnf (vl !! p)
                                 case v0 of
                                   (VGen i') -> case i' == i of
                                                      True -> do let rargs = take p vl ++ drop (p+1) vl
                                                                 bl <- mapM (szSizeVarNotUsed k i) rargs
                                                                 return $ and bl
                                                      False -> return False
                                   _ -> return False
    VApp v1 vl -> do b <- szSizeVarNotUsed k i v1
                     bl <- mapM (szSizeVarDataArgs n k p i) vl
                     return $ b && and bl
    VClos env (Pi x a b) -> do va <-  vclos env a
                               va <- whnf va
                               let k' = k + 1
                               vb <- vclos (update env x (VGen k)) b
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
    VClos env (Pi x a b) -> do va <-  vclos env a
                               va <- whnf va
                               let k' = k + 1
                               vb <- vclos (update env x (VGen k)) b
                               b1 <- szSizeVarNotUsed k' i va
                               b2 <- szSizeVarNotUsed k' i vb
                               return $ b1 && b2
    _ -> return True

doParams :: Int -> Int -> TVal -> TypeCheck (TVal,Int)
doParams 0 k tv = do tv' <- whnf tv
                     return (tv',k)
doParams p k tv = do
  (VClos env (Pi x a b)) <- whnf tv
  bv <- vclos (update env x (VGen k)) b
  doParams (p - 1 ) (k + 1) bv

--------------------------------------
-- check if sizes used correctly in funs and cofuns

-- for inductive funs, every size has to be used at least once in a inductive argument
-- and every argument needs to be either inductive or antitone in the size 
-- the result needs to be monotone in the size
szCheckIndFun :: Int -> TVal -> TypeCheck ()
szCheckIndFun k tv = 
  do tv <- whnf tv
     case tv of
       VClos env (Pi x a b) -> do av <- vclos env a
                                  av <- whnf av
                                  bv <- vclos (update env x (VGen k)) b
                                  let k' = k + 1 
                                  _ <- case av of
                                         VSize -> do g <- szCheckIndFunSize k' k 0 bv
                                                     case g of
                                                       0 -> throwErrorMsg $ "size " ++ x ++ " not used in sized inductive type arg"
                                                       _ -> return ()
                                         _ -> return ()
                                  szCheckIndFun k' bv
       _ -> return ()

-- check and return number of "good" (inductive) arguments
szCheckIndFunSize :: Int -> Int -> Int -> TVal -> TypeCheck Int
szCheckIndFunSize k i good tv = do
  do tv <- whnf tv
     case tv of 
       VClos env (Pi x a b) ->  do av <- vclos env a
                                   av <- whnf av
                                   bv <- vclos (update env x (VGen k)) b
                                   let k' = k + 1 
                                   ind <- szInductive k i av
                                   case ind of
                                     True -> szCheckIndFunSize k i (good + 1) bv
                                     False -> do anti <- szAntitone k i av
                                                 case anti of
                                                   True -> szCheckIndFunSize k i good bv
                                                   False -> throwErrorMsg $ "argument neither inductive nor antitone in " ++ show i
                                                
       _ -> do mon <- szMonotone k i tv
               case mon of
                 True -> return good
                 False -> do coind <- szInductive k i tv
                             case coind of
                               True -> return good
                               False -> throwErrorMsg $ "result not monotone or inductive in size" ++ show i 


-- for a cofun, the size has to be used in a coinductive result type.
-- and all the arguments are either antitone or inductive in the size
szCheckCoFun :: Int -> TVal -> TypeCheck ()
szCheckCoFun k tv = 
  do tv <- whnf tv
     case tv of
       VClos env (Pi x a b) -> do av <- vclos env a
                                  av <- whnf av
                                  bv <- vclos (update env x (VGen k)) b
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
               (DataSig _ _ _ CoInd _) -> 
                   return ()
               _ -> throwErrorMsg "cofun doesn't target coinductive type"
       (VDef n)  -> 
          do sig <- get
             case (lookupSig n sig) of
               (DataSig _ _ _ CoInd _) -> 
                   return ()
               _ -> throwErrorMsg "cofun doesn't target coinductive type"
       _ -> throwErrorMsg "cofun doesn't target coinductive type"

szCheckCoFunSize :: Int -> Int -> TVal -> TypeCheck Bool
szCheckCoFunSize k i tv = -- trace ("szco " ++ show tv) $
  do tv <- whnf tv
     case tv of 
       VClos env (Pi x a b) ->  do av <- vclos env a
                                   av <- whnf av
                                   -- argument must be either antitone or inductive in i 
                                   anti <- szAntitone k i av
                                   case anti of
                                     False -> do ind <- szInductive k i av
                                                 case ind of
                                                   True -> return ()
                                                   False -> throwErrorMsg $ "not antitone or inductive in " ++ show i
                                     True -> return ()
                                   bv <- vclos (update env x (VGen k)) b
                                   let k' = k + 1 
                                   szCheckCoFunSize k' i bv
       -- result must be coinductive
       _ -> do coind <- szCoInductive k i tv
               case coind of
                 True -> return True
                 False -> throwErrorMsg $ "result not coindutive in " ++ show i

szMonotone :: Int -> Int -> TVal -> TypeCheck Bool
szMonotone k i tv = 
 do
   let si = VSucc (VGen i)
   tv' <- substVal [(i,si)] tv
   catchError (do leqVal k tv tv'
                  return True 
              ) (\_ -> return False)

szAntitone :: Int -> Int -> TVal -> TypeCheck Bool
szAntitone k i tv = 
 do
   let si = VSucc (VGen i)
   tv' <- substVal [(i,si)] tv
   catchError (do leqVal k tv' tv
                  return True
              ) (\_ -> return False)


-----
-- checks if tv is a sized inductive type of height i 
szInductive :: Int -> Int -> TVal -> TypeCheck Bool
szInductive k i tv = szUsed' Ind k i tv

-- checks if tv is a sized coinductive type of height i 
szCoInductive :: Int -> Int -> TVal -> TypeCheck Bool
szCoInductive k i tv = szUsed' CoInd k i tv
            
szUsed' :: Co -> Int -> Int -> TVal -> TypeCheck Bool
szUsed' co k i tv =
    do tv <- whnf tv
       case tv of
         (VApp (VDef n) vl) -> 
             do sig <- get
                case (lookupSig n sig) of
                  (DataSig p _ Sized co _) ->                                               
                       szSizeVarArgs k p i vl
                  _ -> return False
         _ -> return False

-- check that arguments are of the form  ... i ..... 
szSizeVarArgs :: Int -> Int -> Int -> [TVal] -> TypeCheck Bool
szSizeVarArgs k p i vl = do case p <= length vl of
                              True -> do 
                                  v0 <- whnf (vl !! p)
                                  case v0 of
                                    (VGen i') | i == i' -> do let rargs = take p vl ++ drop (p+1) vl
                                                              bl <- mapM (szSizeVarNotUsed k i) rargs
                                                              return $ and bl
                                    (VSucc _) -> return False 
                                    _ -> return False
                              False -> return False



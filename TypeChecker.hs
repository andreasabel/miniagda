module TypeChecker where

import Abstract
import Value
import SPos

import Termination

import Completness

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error

import Debug.Trace



runTypeCheck :: Signature -> TypeCheck a -> IO (Either TraceError (a,Signature))
runTypeCheck sig tc = (runErrorT (runStateT tc sig)) 
 
typeCheck dl = runTypeCheck emptySig (typeCheckDecls dl)
               
typeCheckDecls :: [Declaration] -> TypeCheck ()
typeCheckDecls [] = return ()
typeCheckDecls (d:ds) = do typeCheckDeclaration d
                           typeCheckDecls ds
                           return ()

typeCheckDeclaration :: Declaration -> TypeCheck ()
typeCheckDeclaration (DataDecl l) = 
    do bl <- mapM typeCheckDataType l
       zipWithM typeCheckConstructors l bl
       return ()

typeCheckDeclaration (ConstDecl _ (TypeSig n t) e ) = 
   (
    do sig <- get
       checkT t 
       vt <- whnf $ VClos [] t
       checkExpr 0 [] [] e vt
       put (addSig sig n (ConstSig vt e))
   ) `throwTrace` n

typeCheckDeclaration (FunDecl funs) = typeCheckFuns funs


typeCheckDataType :: (Name,Co,[Pos],Telescope,Type,[Constructor]) -> TypeCheck Bool
typeCheckDataType (n,co,pos,tel,t,_) = 
 (    
  do sig <- get
     let dt = (teleToType tel t)
     let params = length tel
     checkDataType 0 [] [] params dt
     v <- whnf $ VClos [] dt
     put $ addSig sig n (DataSig params pos NotSized co v)
     szType params v
 ) `throwTrace` n

typeCheckConstructors :: (Name,Co,[Pos],Telescope,Type,[Constructor]) -> Bool -> TypeCheck ()
typeCheckConstructors (n,co,pos,tel,t,cs) b =
    do bl <- mapM (typeCheckConstructor n pos tel) cs 
       case (b && and bl) of 
             True -> trace (n ++ " is a sized type ") $
                     do sig <- get
                        let DataSig p1 p2 _ p4 p5 = lookupSig n sig
                        put $ addSig sig n (DataSig p1 p2 Sized p4 p5) -- update signature 
             False -> return ()

typeCheckConstructor :: Name -> [Pos] -> Telescope -> Constructor -> TypeCheck Bool
typeCheckConstructor d pos tel (TypeSig n t) = 
   ( 
   do sig <- get 
      let tt = teleToType tel t
      let params = length tel 
      checkConType 0 [] [] params tt
      let (_,target) = typeToTele tt
      checkTarget d tel target
      vt <- whnf $ VClos [] tt
      sposConstructor d 0 pos vt
      put (addSig sig n (ConSig vt)) 
      szConstructor d (length tel) vt
   ) `throwTrace` n




typeCheckFuns :: [(TypeSig,Co, [Clause])] -> TypeCheck ()
typeCheckFuns funs = do mapM addFunSig funs
                        adml <- mapM typeCheckFunSig funs
                        zipWithM typeCheckFunClause [1..] funs 
                        liftIO $ terminationCheck funs adml 
                        mapM enableSig funs      
                        return ()
  where
    addFunSig :: (TypeSig,Co,[Clause]) -> TypeCheck ()
    addFunSig (TypeSig n t,co,cl) = 
        do
          sig <- get 
          vt <- whnf $ VClos [] t
          put (addSig sig n (FunSig co vt cl False)) --not yet type checked / termination checked
    typeCheckFunSig :: (TypeSig,Co,[Clause]) -> TypeCheck [Int] 
    typeCheckFunSig (TypeSig n t,co,cl) =   
       (
        do checkT t
           vt <- whnf $ VClos [] t
           adm <- case co of
                    Ind -> szCheckIndFun 0 [] vt
                    CoInd -> szCheckCoFun 0 [] vt
           b <- case co of
                  Ind -> return $ completeFun cl
                  CoInd -> case adm of 
                    [x] -> return $ completeCoFun cl x
                    _ -> return $ completeFun cl
           case b of
             False -> do 
               liftIO $ putStrLn $ show n ++ " : size pattern incomplete"  
             True -> return ()
           return adm
       ) `throwTrace` ("type of " ++ n)
    typeCheckFunClause :: Int -> (TypeSig,Co,[Clause]) -> TypeCheck ()
    typeCheckFunClause k (TypeSig n t,_,cl) =
       (do checkFun t cl 
       ) `throwTrace` n         
    enableSig :: (TypeSig,Co,[Clause]) -> TypeCheck ()
    enableSig (TypeSig n _,_,_) = do 
                    sig <- get 
                    let (FunSig co vt cl _) = lookupSig n sig 
                    put (addSig sig n (FunSig co vt cl True))
    


-- check that arguments are stypes
-- check that result is a set 
--  ( params were already checked by checkDataType ) 
checkConType :: Int -> Env -> Env -> Int -> Expr -> TypeCheck ()
checkConType k rho gamma p e = 
    case e of
      Pi x t1 t2 -> do 
             case k < p of
               True -> 
                   return ()
               False ->
                   checkSType k rho gamma t1    
             cl <- vclos rho t1 
             checkConType (k+1) (updateV rho x (VGen k)) (updateC gamma x cl) p t2  
      _ -> checkExpr k rho gamma e VSet 
       

-- check that the data type and the parameter arguments (written down like declared in telescope) 
checkTarget :: Name -> Telescope -> Type -> TypeCheck()
checkTarget d tel tg =  case tg of
                           (App (Def n) al) -> if n == d then
                                                   do let pn = length tel
                                                      let params = take pn al
                                                      checkPs tel params -- check parameters              
                                               else throwErrorMsg $ "target mismatch " ++ show tg
                           (Def n) -> if n == d && (null tel) then return () 
                                      else throwErrorMsg $ "target mismatch" ++ show tg
                         
                           _ -> throwErrorMsg $ "target mismatch"  ++ show tg
    where checkPs :: Telescope -> [Expr] -> TypeCheck ()
          checkPs [] [] = return ()
          checkPs ((n,t):tl) ((Var n'):el) = if n == n' then
                                                       checkPs tl el
                                                   else
                                                       throwErrorMsg "target param mismatch"
          checkPs _ _ = throwErrorMsg "target param mismatch"


-- check that params are types
-- check that arguments are stypes
-- check that target is set
checkDataType :: Int -> Env -> Env -> Int -> Expr -> TypeCheck()
checkDataType k rho gamma p e = case e of
                                 Pi x t1 t2 -> do 
                                         case k < p of
                                           True -> 
                                                    checkType k rho gamma t1
                                           False ->
                                                    checkSType k rho gamma t1    
                                         cl <- vclos rho t1 
                                         checkDataType (k+1) (updateV rho x (VGen k)) (updateC gamma x cl) p t2  
                                 Set -> return ()
                                 _ -> throwErrorMsg "doesn't target Set"
                      

------------

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
 do u1f <- force u1
    u2f <- force u2
    case (u1f,u2f) of
      (Just u1',Nothing) | f /= R -> -- only unroll one side  
                   eqVal' L k u1' u2
      (Nothing,Just u2') | f /= L ->
                   eqVal' R k u1 u2' 
      _ -> case (u1,u2) of
             (VSet,VSet) -> return ()
             (VSize,VSize) -> return ()
             (VInfty,VInfty) -> return ()
             (VSucc v1,VSucc v2) -> eqVal' f k v1 v2
             (VApp v1 cls1,VApp v2 cls2) -> do w1 <- mapM whnf cls1
                                               w2 <- mapM whnf cls2
                                               eqVal' f k v1 v2 
                                               eqVals' f k w1 w2
             (VGen k1,VGen k2) -> if k1 == k2 then return ()
                                  else
                                      throwErrorMsg $ "gen mismatch "  ++ show k1 ++ " " ++ show k2 
             (VPi x1 av1 (VClos env1 b1),VPi x2 av2 (VClos env2 b2)) -> 
                        do eqVal' f k av1 av2
                           v1 <- whnf $ VClos (updateV env1 x1 (VGen k)) b1
                           v2 <- whnf $ VClos (updateV env2 x2 (VGen k)) b2 
                           eqVal' f (k+1) v1 v2
             (VLam x1 (VClos env1 b1), VLam x2 (VClos env2 b2)) ->
                        do v1 <- whnf $ VClos (updateV env1 x1 (VGen k)) b1
                           v2 <- whnf $ VClos (updateV env2 x2 (VGen k)) b2
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
 do u1f <- force u1
    u2f <- force u2
    case (u1f,u2f) of
      (Just u1',Nothing) | f /= R -> -- only unroll one side  
                   leqVal' L k u1' u2
      (Nothing,Just u2') | f /= L ->
                   leqVal' R k u1 u2'    
      _ ->  case (u1,u2) of
              (VSet,VSet) -> return ()
              (VSize,VSize) -> return ()
              (VInfty,VInfty) -> return ()
              (VSucc v1,VSucc v2) -> leqVal' f k v1 v2
              (VApp v1 cls1,VApp v2 cls2) -> do
                           w1 <- mapM whnf cls1
                           w2 <- mapM whnf cls2
                           leqApp f k v1 w1 v2 w2
              (VGen k1,VGen k2) -> if k1 == k2 then return () 
                                   else throwErrorMsg $ "gen mismatch "  ++ show k1 ++ " " ++ show k2 
              (VPi x1 av1 (VClos env1 b1), VPi x2 av2 (VClos env2 b2)) -> 
                 do leqVal' (switch f) k av2 av1
                    bv1 <- whnf $ VClos (updateV env1 x1 (VGen k)) b1
                    bv2 <- whnf $ VClos (updateV env2 x2 (VGen k)) b2
                    leqVal' f (k+1) bv1 bv2
              (VLam x1 (VClos env1 e1), VLam x2 (VClos env2 e2)) ->
                  do v1 <- whnf $ VClos (updateV env1 x1 (VGen k)) e1
                     v2 <- whnf $ VClos (updateV env2 x2 (VGen k)) e2
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
                                                                    leqSize (w1 !! p) (w2 !! p)
                                                                CoInd ->
                                                                    leqSize (w2 !! p) (w1 !! p)
                                                         NotSized -> 
                                                                    eqVal k (w1 !! p) (w2 !!p)
                                                   False -> return ()
                                         _ -> do leqVal k v1 v2 
                                                 eqVals k w1 w2
                                                
        _ -> do leqVal k v1 v2 
                eqVals k w1 w2




-- substaging on size values
leqSize :: Val -> Val -> TypeCheck ()
leqSize v1 v2 = --trace ("leqSize " ++ show v1 ++ show v2) $ 
    do 
       case (v1,v2) of
         (_,VInfty) -> return ()
         (VSucc v1,VSucc v2) -> leqSize v1 v2
         (v1,VSucc v2) -> leqSize v1 v2
         (VGen v1,VGen v2) | v1 == v2 -> return ()
         _ -> throwErrorMsg $ "leqSize " ++ show v1 ++ " " ++ show v2  
    
-- type checking

checkExpr :: Int -> Env -> Env -> Expr -> TVal -> TypeCheck ()
checkExpr k rho gamma e v = --trace ("checkExpr " ++ show e ++ " " ++ show v) $
    case (e,v) of
      (Lam n e1,VPi x va (VClos env t1)) -> 
          do
            v_t1 <- whnf $ VClos (updateV env x (VGen k)) t1
            checkExpr (k+1) (updateV rho n (VGen k)) (updateV gamma n va) e1 v_t1  
      (Pi n t1 t2,VSet) ->
          do checkExpr k rho gamma t1 VSet
             cl <- vclos rho t1 
             checkExpr (k+1) (updateV rho n (VGen k)) (updateC gamma n cl) t2 VSet
      (Succ e2,VSize) -> checkExpr k rho gamma e2 VSize
      _ -> do v2 <- inferExpr k rho gamma e
              leqVal k v2 v

inferExpr :: Int -> Env -> Env -> Expr -> TypeCheck TVal 
inferExpr k rho gamma e = 
    --trace ("inferExpr " ++ show e) $
    case e of
      Var x -> lookupEnv gamma x
      Size -> return VSet
      Infty -> return VSize
      App e1 [e2] ->
          do
            v <- inferExpr k rho gamma e1
            case v of
               VPi x av (VClos env b) ->  do checkExpr k rho gamma e2 av 
                                             v2 <- vclos rho e2
                                             whnf $ VClos (updateC env x v2) b  
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

      
checkT :: Expr -> TypeCheck ()
checkT e = checkType 0 [] [] e 

checkType :: Int -> Env -> Env -> Expr -> TypeCheck ()
checkType k rho gamma e = case e of
                Set -> return ()
                Pi x t1 t2 -> do
                    checkType k rho gamma t1
                    cl <- vclos rho t1 
                    checkType (k+1) (updateV rho x (VGen k)) (updateC gamma x cl) t2  
                _ -> checkExpr k rho gamma e VSet


-- set type
checkSType :: Int -> Env -> Env -> Expr -> TypeCheck ()
checkSType k rho gamma e  = (checkExpr k rho gamma e VSet) `catchError` (\_ -> throwErrorMsg $ "not a set: " ++ show e )
     

-- type check a funtion

checkFun = checkFun' 1 

checkFun' :: Int -> Type -> [Clause] -> TypeCheck ()
checkFun' i t [] = return ()
checkFun' i t (c:cl) = do checkClause i t c
                          checkFun' (i + 1) t cl

checkClause :: Int -> Type -> Clause -> TypeCheck ()
checkClause i t (Clause pl rhs) =
    (
    do 
      v <- whnf $ VClos [] t
      (k,flex,ins,rho,gamma,vt) <- checkPatterns 0 [] [] [] [] v pl
      mapM (checkDot k rho gamma ins) flex
      checkRHS k rho gamma rhs vt
    ) `throwTrace` ("clause " ++ show i)

type Substitution = [(Int,TVal)]


checkPatterns :: Int -> [(Int,(Expr,TVal))] -> Substitution -> Env -> Env -> TVal -> [Pattern] -> TypeCheck (Int,[(Int,(Expr,TVal))],Substitution,Env,Env,TVal)
checkPatterns k flex ins rho gamma v pl = 
    case pl of
      [] -> return (k,flex,ins,rho,gamma,v)
      (p:pl') -> do (k',flex',ins',rho',gamma',v') <- checkPattern k flex ins rho gamma v p 
                    checkPatterns k' flex' ins' rho' gamma' v' pl' 
       
{- 
checkPattern k flex subst rho gamma v p = (k', flex', subst', rho', gamma', v')

Input : 
  k     : next free generic value
  flex  : list of pairs (flexible variable, its dot pattern + supposed type)
  subst : list of pairs (flexible variable, its valuation)
  rho   : binding of variables to values
  gamma : binding of variables to their types
  v     : type of the expression \ p -> t
  p     : the pattern to check

Output
  v'    : type of t
-}

checkPattern :: Int -> [(Int,(Expr,TVal))] -> Substitution -> Env -> Env -> TVal -> Pattern -> TypeCheck (Int,[(Int,(Expr,TVal))],Substitution,Env,Env,TVal)
checkPattern k flex ins rho gamma v p = -- trace ("cp " ++ show k ++ " " ++ show flex ++ " " ++ show rho ++ " " ++ show gamma ++ " " ++ show v ++ " " ++ show p) $ 
 (
 do 
  case v of
    VPi x av (VClos env b) -> do
       case p of
          VarP y -> do let gk = VGen k 
                       bv <- whnf $ VClos (updateV env x gk) b 
                       return (k+1
                              ,flex
                              ,ins
                              ,updateV rho y gk
                              ,updateV gamma y av
                              ,bv
                              )
          ConP co n pl -> do 
                 sig <-  get 
                 let (ConSig vc) = (lookupSig n sig)
                 (k',flex',ins',rho',gamma',vc') <- checkPatterns k flex ins rho gamma vc pl
                 let flexgen = fst $ unzip flex'
                 subst <- instantiate k' flexgen vc' av
                 pc <- patternToClos k rho' flexgen p
                 vb <- whnf $ VClos (updateC env x pc) b
                 ins'' <- compSubst ins' subst
                 vb <- substVal ins'' vb
                 gamma' <- substEnv ins'' gamma'
                 return (k',flex',ins'',rho',gamma',vb)
          SuccP p2 -> do  
                         st <- whnf $ VClos [] (Pi "" Size Size) 
                         (k',flex',ins',rho',gamma',v') <- checkPattern  k flex ins rho gamma st p2
                         eqVal k' av v'
                         let flexgen = fst $ unzip flex' 
                         pv <- patternToClos k rho' flexgen p
                         vb <- whnf $ VClos (updateC env x pv) b
                         return (k',flex',ins',rho',gamma',vb)
          DotP e -> do vb <- whnf $ VClos (updateV env x (VGen k)) b
                       return (k+1
                              ,(k,(e,av)):flex
                              ,ins
                              ,rho
                              ,gamma
                              ,vb)
          _ -> throwErrorMsg $ "checkpattern " ++ show v 
    _ -> throwErrorMsg $ "checkpattern " ++ show v
  ) `throwTrace` ("pattern " ++ show p)
 

patternToClos :: Int -> Env -> [Int] -> Pattern -> TypeCheck Clos
patternToClos k rho fl p = do (e,_) <- patternToExpr k p
                              let env = dotEnv rho k fl 
                              return $ VClos env e

dotEnv :: Env -> Int -> [Int] -> Env
dotEnv env k [] = env
dotEnv env k (f:flex) = if f < k then dotEnv env k flex
                    else ("d$"++show f,V $ VGen f):(dotEnv env k flex) -- d$i is fresh

-- turn a pattern into a Expression
-- dot patterns get variables corresponding to their flexible generic value
patternToExpr :: Int -> Pattern -> TypeCheck (Expr,Int)
patternToExpr k p = 
    case p of
      VarP n -> return $ (Var n,k+1)
      ConP co n [] -> return $ (Con co n,k)
      ConP co n pl -> do
             (el,k') <- patternsToExpr k pl
             return $ (App (Con co n) el,k')
      SuccP p -> do (e,k') <- patternToExpr k p
                    return $ (Succ e,k')   
      DotP e ->  return $ (Var ("d$" ++ show k),k+1)

patternsToExpr :: Int -> [Pattern] -> TypeCheck ([Expr],Int)
patternsToExpr k [] = return $ ([],k)
patternsToExpr k (p:pl) = do (e,k') <- patternToExpr k p
                             (el,k'') <- patternsToExpr k' pl
                             return (e:el,k'')


checkDot :: Int -> Env -> Env -> Substitution -> (Int,(Expr,TVal)) -> TypeCheck()
checkDot k rho gamma subst (i,(e,tv)) =  --trace ("checking dot pattern " ++ show e) $ 
                                         (
                                         case (lookup i subst) of
                                           Nothing -> throwErrorMsg $ "not instantiated "
                                           Just v -> do tv <- substVal subst tv
                                                        checkExpr k rho gamma e tv
                                                        v' <-  whnf $ VClos rho e
                                                        eqVal k v v'
                                         ) `throwTrace` ("dot pattern " ++ show e )            
checkRHS :: Int -> Env -> Env -> Expr -> TVal -> TypeCheck ()
checkRHS k rho gamma rhs v =  --trace ("checking rhs") $ 
                     (
                      checkExpr k rho gamma rhs v  
                     ) `throwTrace` "right hand side"


-- instantiate the parameters and family indices
-- this yields a substitutin for the flexible variables 
instantiate :: Int -> [Int] -> TVal -> TVal -> TypeCheck Substitution
instantiate m flex v1 v2 = do subst <- inst m flex v1 v2
                              --flattenSubst subst
                              return subst
                                     
inst :: Int -> [Int] -> Val -> Val -> TypeCheck Substitution
inst m flex v1 v2 =  do
  case (v1,v2) of
    (VGen k,_) | elem k flex -> return [(k,v2)]
    (_,VGen k) | elem k flex -> return [(k,v1)]
    (VApp (VDef d1) cls1,VApp (VDef d2) cls2) | d1 == d2 -> 
         do vl1 <- mapM whnf cls1
            vl2 <- mapM whnf cls2
            instList m flex vl1 vl2
    (VApp (VCon _ c1) cls2,VApp (VCon _ c2) cls1) | c1 == c2 -> 
         do vl1 <- mapM whnf cls1
            vl2 <- mapM whnf cls2
            instList m flex vl1 vl2
    (VSucc v1',VSucc v2') -> inst m flex v1' v2'
    (VSucc v, VInfty) -> inst m flex v VInfty
    _ -> do eqVal m v1 v2
            return []
   
instList :: Int -> [Int] -> [Val] -> [Val] -> TypeCheck Substitution
instList m flex [] [] = return []
instList m flex (v1:vl1) (v2:vl2) = do map <- inst m flex v1 v2
                                       vl1' <- mapM (substVal map) vl1
                                       vl2' <- mapM (substVal map) vl2
                                       map' <- instList m flex vl1' vl2'
                                       compSubst map map'

-- substitute generic variable in value
substVal :: Substitution -> TVal -> TypeCheck Val
substVal map v =  
  do
    case v of
      VGen k -> case lookup k map of
                  Nothing -> return v
                  Just v' -> return v'
      VApp v1 cls -> do v1' <- substVal map v1
                        cls' <- mapM (substClos map) cls
                        return $ VApp v1' cls'
      VSucc v1 -> do v1' <- substVal map v1
                     return $ sinfty v1'
      VPi x av (VClos env b) -> do av' <- substVal map av
                                   env' <- substEnv map env
                                   return $ VPi x av' (VClos env' b)
      VLam x (VClos env b) -> do env' <- substEnv map env
                                 return $ VLam x (VClos env' b)
      _ -> return v

substClos :: Substitution -> Clos -> TypeCheck Clos
substClos map (VClos env e) = do env' <- substEnv map env
                                 return $ VClos env' e

-- substitute in environment
substEnv :: Substitution -> Env -> TypeCheck Env
substEnv map [] = return []
substEnv map ((x,V v):env) = do v' <- substVal map v
                                env' <- substEnv map env
                                return $ (x,V v'):env'
substEnv map ((x,C c):env) = do c' <- substClos map c
                                env' <- substEnv map env
                                return $ (x,C c'):env'



-- "merge" substitutions by first applying the second to the first, then 
-- appending them
compSubst :: Substitution -> Substitution -> TypeCheck Substitution
compSubst subst1 subst2 =
    do let (dom1,tg1) = unzip subst1
       tg1' <- mapM (substVal subst2) tg1
       let subst1' = zip dom1 tg1'
       return $ subst1' ++ subst2               

---- Size checking
--------------

-- check wether the data type is sized type


-- check data declaration type 
-- parameters : number of params, type 
szType :: Int -> TVal -> TypeCheck Bool
szType p tv = do (tv',k) <- doVParams p 0 tv
                 case tv' of
                   VPi x av (VClos env e) -> do
                      case av of
                         VSize -> return True
                         _ -> return False
                   _ -> return False
                 
-- check data constructors
szConstructor :: Name -> Int -> TVal -> TypeCheck Bool
szConstructor n p tv = do (tv',k) <- doVParams p 0 tv
                          case tv' of
                            VPi x av (VClos env b) -> do
                                     case av of
                                       VSize -> do bv <- whnf $ VClos (updateV env x (VGen k)) b 
                                                   szSizeVarUsage n (k+1) p k bv
                                       _ -> return False
                            _ -> return False

szSizeVarUsage :: Name -> Int -> Int -> Int -> TVal -> TypeCheck Bool 
szSizeVarUsage n k p i tv = case tv of
                                 VPi x av (VClos env b) -> do
                                          b1 <- szSizeVarDataArgs n k p i av
                                          bv <- whnf $ VClos (updateV env x (VGen k)) b
                                          b2 <- szSizeVarUsage n (k+1) p i bv
                                          return $ b1 && b2
                                 _ -> szSizeVarTarget k p i tv


-- check that Target is of form D ... (Succ i) ... 
szSizeVarTarget :: Int -> Int -> Int -> TVal -> TypeCheck Bool
szSizeVarTarget k p i (VApp v1 cls) = do
                             vl <- mapM whnf cls
                             let v0 = (vl !! p)
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
   case tv of
    VGen i' -> return $ i' /= i
    VSucc tv' -> szSizeVarDataArgs n k p i tv'
    VApp (VDef m) cls | n == m -> do 
                                 vl <- mapM whnf cls
                                 let v0 = vl !! p
                                 case v0 of
                                   (VGen i') -> case i' == i of
                                                      True -> do let rargs = take p vl ++ drop (p+1) vl
                                                                 bl <- mapM (szSizeVarNotUsed k i) rargs
                                                                 return $ and bl
                                                      False -> return False
                                   _ -> return False
    VApp v1 cls -> do b <- szSizeVarNotUsed k i v1
                      vl <- mapM whnf cls
                      bl <- mapM (szSizeVarDataArgs n k p i) vl
                      return $ b && and bl
    VPi x av (VClos env b) -> do 
                               let k' = k + 1
                               bv <- whnf $ VClos (updateV env x (VGen k)) b
                               b1 <- szSizeVarDataArgs n k' p i av
                               b2 <- szSizeVarDataArgs n k' p i bv
                               return $ b1 && b2
    _ -> return True


szSizeVarNotUsed :: Int -> Int -> TVal -> TypeCheck Bool
szSizeVarNotUsed k i tv = do
  case tv of
    VGen i' -> return $ i' /= i
    VSucc tv' -> szSizeVarNotUsed k i tv'
    VApp v1 cls -> do b <- szSizeVarNotUsed k i v1
                      vl <- mapM whnf cls
                      bl <- mapM (szSizeVarNotUsed k i) vl
                      return $ b && and bl
    VPi x av (VClos env b) -> do 
                               let k' = k + 1
                               bv <- whnf $ VClos (updateV env x (VGen k)) b
                               b1 <- szSizeVarNotUsed k' i av
                               b2 <- szSizeVarNotUsed k' i bv
                               return $ b1 && b2
    _ -> return True

doVParams :: Int -> Int -> TVal -> TypeCheck (TVal,Int)
doVParams 0 k tv = do return (tv,k)
doVParams p k (VPi x av (VClos env b)) = do 
  bv <- whnf $ VClos (updateV env x (VGen k)) b
  doVParams (p - 1 ) (k + 1) bv

--------------------------------------
-- check for admissible size argumments

-- for inductive fun, for an admissble size argument i
-- - every argument needs to be either inductive or antitone in i 
-- - the result needs to be monotone or coinductive in i 

-- returns the indices of the adm size arguments
szCheckIndFun :: Int -> [Int] -> TVal -> TypeCheck [Int]
szCheckIndFun k adm tv = 
      case tv of
       VPi x av (VClos env b) -> do bv <- whnf $ VClos (updateV env x (VGen k)) b
                                    let k' = k + 1 
                                    adm' <- case av of
                                              VSize -> do g <- szCheckIndFunSize k' False k bv
                                                          case g of
                                                            True  -> return $ k:adm
                                                            False -> return adm
                                              _ -> return adm
                                    szCheckIndFun k' adm' bv
       _ -> return adm

-- g == true if at leat one argument was inductive in i 
szCheckIndFunSize :: Int -> Bool -> Int -> TVal -> TypeCheck Bool
szCheckIndFunSize k g i tv = do
  case tv of 
       VPi x av (VClos env b) ->  do 
                                   bv <- whnf $ VClos (updateV env x (VGen k)) b
                                   let k' = k + 1 
                                   ind <- szInductive k i av
                                   case ind of
                                     True -> szCheckIndFunSize k True i bv
                                     False -> do anti <- szAntitone k i av
                                                 case anti of
                                                   True -> szCheckIndFunSize k g i bv
                                                   False -> return False
       _ -> do mon <- szMonotone k i tv
               case mon of
                 True -> return g
                 False -> do ind <- szCoInductive k i tv
                             case ind of
                               True -> return g
                               False -> return False
                                      


-- for nonrecursive fun, for every admissble size argument i
-- every argument needs to be either inductive or antitone in i 

-- returns the indices of the adm size arguments 
-- (either empty or a singleton list)
szCheckCoFun :: Int -> [Int] -> TVal -> TypeCheck [Int]
szCheckCoFun k adm tv = 
      case tv of
       VPi x av (VClos env b) -> do 
                                  bv <- whnf $ VClos (updateV env x (VGen k)) b
                                  let k' = k + 1 
                                  adm' <- case av of
                                         VSize -> do b <- szCheckCoFunSize k' k bv
                                                     case b of
                                                       False -> return adm
                                                       True -> return $ k:adm
                                         _ ->  return adm
                                  szCheckCoFun k' adm' bv
       -- result 
       (VApp (VDef n) vl) -> 
          do sig <- get
             case (lookupSig n sig) of
               (DataSig _ _ _ CoInd _) -> 
                   return adm
               _ -> throwErrorMsg "cofun doesn't target coinductive type"
       (VDef n)  -> 
          do sig <- get
             case (lookupSig n sig) of
               (DataSig _ _ _ CoInd _) -> 
                   return adm
               _ -> throwErrorMsg "cofun doesn't target coinductive type"
       _ -> throwErrorMsg "cofun doesn't target coinductive type"

szCheckCoFunSize :: Int -> Int -> TVal -> TypeCheck Bool
szCheckCoFunSize k i tv = -- trace ("szco " ++ show tv) $
      case tv of 
       VPi x av (VClos env b) ->  do 
                                   -- argument must be either antitone or inductive in i 
                                   anti <- szAntitone k i av
                                   ind <- szInductive k i av
                                   case (anti || ind) of
                                     True ->
                                         do bv <- whnf $ VClos (updateV env x (VGen k)) b
                                            let k' = k + 1 
                                            szCheckCoFunSize k' i bv
                                     False -> return False
       -- result must be coinductive
       _ -> szCoInductive k i tv
              

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
-- checks if tv is a sized inductive type of size i 
szInductive :: Int -> Int -> TVal -> TypeCheck Bool
szInductive k i tv = szUsed' Ind k i tv

-- checks if tv is a sized coinductive type of size i 
szCoInductive :: Int -> Int -> TVal -> TypeCheck Bool
szCoInductive k i tv = szUsed' CoInd k i tv
            
szUsed' :: Co -> Int -> Int -> TVal -> TypeCheck Bool
szUsed' co k i tv =
    case tv of
         (VApp (VDef n) cls) -> 
             do sig <- get
                case (lookupSig n sig) of
                  DataSig p _ Sized co' _ | co == co' && (length cls) <= p + 1 -> 
                      do s <- whnf (cls !! p)
                         case s of
                           VGen i' | i == i' -> return True
                           _ -> return False 
                  _ -> return False
         _ -> return False


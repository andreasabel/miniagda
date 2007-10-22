module Value where

import Abstract

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Debug.Trace

-- reader monad for local environment (not used right now)
-- state monad for global signature
type TypeCheck a = ReaderT Env (StateT Signature (ErrorT TraceError Identity)) a 

data TraceError = Err String | TrErr String TraceError

instance Error TraceError where
    noMsg = Err "no message"
    strMsg s = Err s

instance Show TraceError where
    show (Err str) = str
    show (TrErr str err) = str ++ " -> \n" ++ show err 

throwErrorMsg m = throwError (Err m) 

throwTrace x n = x `catchError` ( \e -> throwError $ TrErr n e) 


data Val =   VSet
           | VSize
           | VInfty
           | VSucc Val   -- VSucc Env Expr
           | VApp Val [Clos]
           | VCon Co Name
           | VDef Name
           | VGen Int
           | VPi Name Val Clos
           | VLam Name Clos
             deriving (Eq)

data Clos = VClos Env Expr -- VClos
          deriving (Eq)

instance Show Clos where
    show = showClos

type TVal = Val

instance Show Val where
    show = showVal

showVal :: Val -> String
showVal VSet = "Set"
showVal VSize = "Size"
showVal VInfty = "#"
showVal (VSucc v) = "($ " ++ showVal v ++ ")" 
showVal (VApp v cls) = "(" ++ showVal v ++ " " ++ showCloss cls ++ ")"
    where
      showCloss :: [Clos] -> String
      showCloss [] = ""
      showCloss (cl:cls) = showClos cl ++ (if null cls then "" else " " ++ showCloss cls)   
showVal (VCon _ n) = n
showVal (VDef n) = n
showVal (VGen k) = show k

showVal (VPi x av cl) = "( " ++ x  ++ " : " ++ showVal av ++ ") ->" ++ showClos cl ++ ")"
showVal (VLam x cl) = "(\\" ++ x ++ " -> " ++ showClos cl ++ ")" 

showClos :: Clos -> String
showClos (VClos env e) = showEnv env ++ prettyExpr e

showEnv :: Env -> String
showEnv [] = ""
showEnv x = "{" ++ showEnv' x ++ "} " where
    showEnv' [] = []
    showEnv' ((n,V v):env) = "(" ++ n ++ " = " ++ showVal v ++ ")" ++ showEnv' env 
    showEnv' ((n,C c):env) = "(" ++ n ++ " = " ++ showClos c ++ ")" ++ showEnv' env                           

runWhnfClos :: Env -> Signature -> TypeCheck a -> Either TraceError (a,Signature)
runWhnfClos env sig tc = runIdentity (runErrorT (runStateT (runReaderT tc env) sig)) 
 
whnfClos sig v = runWhnfClos emptyEnv sig (prettyVal =<< whnf v)


prettyVal :: Val  -> TypeCheck String
prettyVal VSet = return "Set"
prettyVal VSize = return "Size"
prettyVal VInfty = return "#"
prettyVal (VSucc v) = do pv <- prettyVal v
                         return $ "($" ++ pv ++ ")" 
-- don't whnf arguments of a coinductive constructor
prettyVal (VApp v@(VCon CoInd _) cls) = do pv <- prettyVal v
                                           pcls <- mapM prettyClos cls
                                           let pcls' = showStrs pcls
                                           return $ "(" ++ pv ++ " " ++ pcls' ++ ")"
prettyVal (VApp v cls) = do pv <- prettyVal v
                            vl <- mapM whnf cls
                            pvl <- mapM prettyVal vl
                            let pvl' = showStrs pvl
                            return $ "(" ++ pv ++ " " ++ pvl' ++ ")"
prettyVal (VCon _ n) = return n
prettyVal (VDef n) = return n
prettyVal (VGen k) = return $ show k
prettyVal (VPi x av cl) = return $ "( " ++ x  ++ " : " ++ showVal av ++ ") ->" ++ showClos cl ++ ")"
prettyVal (VLam x cl) = return $ "(\\" ++ x ++ " -> " ++ showClos cl ++ ")"

prettyClos (VClos env e) = return $ showEnv env ++ prettyExpr e
      
showStrs :: [String] -> String
showStrs [] = ""
showStrs (s:sl) = s ++ (if null sl then "" else " " ++ showStrs sl)

----

data ClosVal = V Val | C Clos
               deriving Eq

type Env = [(Name,ClosVal)]

emptyEnv = []

updateC :: Env -> Name -> Clos -> Env
updateC env "" c = env
updateC env n c = (n,C c):env

updateV :: Env -> Name -> Val -> Env
updateV env "" v = env
updateV env n v = (n,V v):env

lookupEnv :: Env -> Name -> TypeCheck Val
lookupEnv [] n = throwErrorMsg $ "lookup env error " ++ n 
lookupEnv ((x,cv):xs) n = if x == n then case cv of
                                           V v -> return v
                                           C c -> do v <- whnf c
                                                     return v
                          else lookupEnv xs n


--


type Signature = Map.Map Name SigDef

data Sized = Sized | NotSized 
             deriving (Eq,Show)



data SigDef = FunSig Co TVal [Clause] --type , co , clauses
            | ConstSig TVal Expr -- type , expr 
            | ConSig TVal -- type   
            | DataSig Int [Pos] Sized Co TVal -- # parameters, positivity of parameters  , sized , co , type  
              deriving (Show)

emptySig = Map.empty

lookupSig :: Name -> Signature -> SigDef
lookupSig n sig = case (Map.lookup n sig) of
                       Nothing -> error $ "Error not in signature: "  ++ n ++ " " ++ show sig
                       Just k -> k

addSig :: Signature -> Name -> SigDef -> Signature
addSig sig n def = Map.insert n def sig

---


whnf :: Clos -> TypeCheck Val
whnf c@(VClos env e) = --trace ("whnf " ++ show
  case e of
    Lam x e1 -> return $ VLam x (VClos env e1)
    Pi x a b -> do av <- whnf (VClos env a)
                   return $ VPi x av (VClos env b)
    App e1 e2 -> do v1 <- whnf (VClos env e1)
                    app v1 (map (VClos env) e2)
    Set -> return  VSet
    Infty -> return VInfty
    Succ e1 -> do v <- whnf (VClos env e1)
                  return $ sinfty v
                
    Size -> return VSize
    Con co n -> return $ VCon co n
    
    Def n -> return $ VDef n
    Const n -> do sig <- get 
                  let (ConstSig _ e) = lookupSig n sig
                  whnf (VClos [] e)
    Var y -> lookupEnv env y

sinfty :: Val -> Val
sinfty v = case v of
            VInfty -> VInfty
            _ -> VSucc v               

vclos :: Env -> Expr -> TypeCheck Clos
vclos env e = return $ VClos env e

app :: Val -> [Clos] -> TypeCheck Val
app u [] = return $ u
app u c = do  
         case (u,c) of
            (VApp u2 c2,_) -> app u2 (c2 ++ c)
            (VLam x (VClos env e),(cl:cls))  -> do v' <- whnf (VClos (updateC env x cl) e)
                                                   app v' cls
            (VDef n,_) -> appDef n c
            _ -> return $ VApp u c

-- unroll a corecursive definition once
force :: Val -> TypeCheck (Maybe Val)
force v@ (VDef n) = --trace ("force " ++ show v) $
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t cl) -> matchClauses cl []
                                  
         _ -> return Nothing
force v@(VApp (VDef n) vl) = --trace ("force " ++ show v) $
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t cl) -> matchClauses cl vl
         _ -> return Nothing
force v = return Nothing


appDef :: Name -> [Clos] -> TypeCheck Val
appDef n vl = --trace ("appDef " ++ n) $ 
    do
      sig <- get
      case lookupSig n sig of
        (FunSig Ind _ cl) -> do m <- matchClauses cl vl
                                case m of
                                  Nothing -> return $ VApp (VDef n) vl
                                  Just v2 -> return v2
        _ -> return $ VApp (VDef n) vl   

---- Pattern Matching ----

matchClauses :: [Clause] -> [Clos] -> TypeCheck (Maybe Val)
matchClauses cl cll = loop cl
    where loop [] = return Nothing
          loop  (Clause pl rhs : cl2) = 
              do x <- matchClause [] pl rhs cll 
                 case x of
                   Nothing -> loop cl2
                   Just v -> return $ Just v

matchClause :: Env -> [Pattern] -> Expr -> [Clos] -> TypeCheck (Maybe Val)
matchClause env [] rhs cll = do v <- whnf (VClos env rhs)
                                v2 <- app v cll
                                return $ Just v2
matchClause env (p:pl) rhs (cl:cls) =
 do  v <- whnf cl
     m <- match env p v 
     case m of
       Just env' ->
         matchClause env' pl rhs cls
       Nothing -> return Nothing
-- too few arguments
matchClause _ _ _ [] = return Nothing

match :: Env -> Pattern -> Val-> TypeCheck (Maybe Env)
match env p v = --trace (n ++ " :" ++ show p ++ show v) $ 
  do 
    -- force against coinductive constructor pattern
    v <- case p of
           ConP CoInd _ _ -> do f <- force v
                                case f of
                                  Nothing -> return v
                                  Just v' -> return v'
           _ -> return v
    case (p,v) of
      (DotP _,_) -> return $ Just env
      (VarP x,_) -> return $ Just (updateV env x v)
      (ConP _ x [],VCon _ y) | x== y -> return $ Just env
      (ConP _ x pl,VApp (VCon _ y) cll) | x == y -> matchList env pl cll
      (_,VInfty) -> matchInfty env p 
      (SuccP p',VSucc v') -> match env p' v'
      _ -> return Nothing

matchInfty :: Env -> Pattern -> TypeCheck (Maybe Env)
matchInfty env p = 
    case p of
        DotP _ -> return $ Just env
        VarP x -> return $ Just $ updateV env x VInfty
        SuccP p' -> matchInfty env p'
        _ -> return Nothing

matchList :: Env -> [Pattern] -> [Clos] -> TypeCheck (Maybe Env)
matchList env [] [] = return $ Just env
matchList env (p:pl) (cl:cll) = do v <- whnf cl
                                   m <- match env p v 
                                   case m of 
                                     Just env' -> matchList env' pl cll
                                     Nothing -> return Nothing
matchList _ _ _ = return Nothing



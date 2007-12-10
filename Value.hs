module Value where

import Abstract

import TraceError

import qualified Data.Map as Map

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error

import Debug.Trace

-- reader monad for local environment (not used right now)
-- state monad for global signature
type TypeCheck a = StateT Signature (ErrorT TraceError IO) a 


data Val =   VSet
           | VSize
           | VInfty
           | VSucc Val   
           | VApp Val [Val]
           | VCon Co Name
           | VDef Name
           | VGen Int
           | VPi Name Val Env Expr
           | VLam Name Env Expr
             deriving (Eq)

type TVal = Val

instance Show Val where
    show = showVal

showVal :: Val -> String
showVal VSet = "Set"
showVal VSize = "Size"
showVal VInfty = "#"
showVal (VSucc v) = "($ " ++ showVal v ++ ")" 
showVal (VApp v vl) = "(" ++ showVal v ++ " " ++ showVals vl ++ ")"
    where
      showVals :: [Val] -> String
      showVals [] = ""
      showVals (v:vl) = showVal v ++ (if null vl then "" else " " ++ showVals vl)   
showVal (VCon _ n) = n
showVal (VDef n) = n
showVal (VGen k) = show k

showVal (VPi x av env e) = "( " ++ x  ++ " : " ++ showVal av ++ ") ->" ++ showEnv env ++ show e ++ ")"
showVal (VLam x env e) = "(\\" ++ x ++ " -> " ++ showEnv env ++ show e ++ ")" 

showEnv :: Env -> String
showEnv [] = ""
showEnv x = "{" ++ showEnv' x ++ "} " where
    showEnv' [] = []
    showEnv' ((n,v):env) = "(" ++ n ++ " = " ++ showVal v ++ ")" ++ showEnv' env 
   
runWhnf :: Signature -> TypeCheck a -> IO (Either TraceError (a,Signature))
runWhnf sig tc = (runErrorT (runStateT tc  sig)) 
 
doWhnf sig v = runWhnf sig (whnf [] v)

----

type Env = [(Name,Val)]

emptyEnv = []

update :: Env -> Name -> Val -> Env
update env "" v = env
update env n v = (n,v):env

lookupEnv :: Env -> Name -> Val
lookupEnv [] n = error $ "lookup env error " ++ n 
lookupEnv ((x,v):xs) n = if x == n then v 
                          else lookupEnv xs n

--


type Signature = Map.Map Name SigDef

data SigDef = FunSig Co TVal [Clause] Bool --type , co , clauses , wether its type checked
            | LetSig TVal Expr -- type , expr 
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


whnf :: Env -> Expr -> TypeCheck Val
whnf env e= --trace ("whnf " ++ show
  case e of
    LLet x t1 e1 e2 -> do v1 <- whnf env e1
                          whnf (update env x v1) e2
    Lam x e1 -> return $ VLam x env e1
    Pi x a b -> do av <- whnf env a
                   return $ VPi x av env b
    App e1 el -> do v1 <- whnf env e1
                    vl <- mapM (whnf env) el
                    app v1 vl
    Set -> return  VSet
    Infty -> return VInfty
    Succ e1 -> do v <- whnf env e1
                  return $ sinfty v
                
    Size -> return VSize
    Con co n -> return $ VCon co n
    
    Def n -> return $ VDef n
    Let n -> do sig <- get 
                let (LetSig _ e) = lookupSig n sig
                whnf [] e
    Var y -> return $ lookupEnv env y

sinfty :: Val -> Val
sinfty v = case v of
            VInfty -> VInfty
            _ -> VSucc v               

app :: Val -> [Val] -> TypeCheck Val
app u [] = return $ u
app u c = do  
         case (u,c) of
            (VApp u2 c2,_) -> app u2 (c2 ++ c)
            (VLam x env e,(v:vl))  -> do v' <- whnf (update env x v) e
                                         app v' vl
            (VDef n,_) -> appDef n c
            _ -> return $ VApp u c

-- unroll a corecursive definition one time (until constructor appears)
force :: Val -> TypeCheck Val
force v@ (VDef n) = --trace ("force " ++ show v) $
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t cl True) -> do m <- matchClauses cl []
                                        case m of 
                                          Just v' -> force v'
                                          Nothing -> return v
         _ -> return v
force v@(VApp (VDef n) vl) = --trace ("force " ++ show v) $
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t cl True) ->  do m <- matchClauses cl vl
                                         case m of 
                                           Just v' -> force v'
                                           Nothing -> return v
         _ -> return v
force v = return v


appDef :: Name -> [Val] -> TypeCheck Val
appDef n vl = --trace ("appDef " ++ n) $ 
    do
      sig <- get
      case lookupSig n sig of
        (FunSig Ind _ cl True) -> do m <- matchClauses cl vl
                                     case m of
                                       Nothing -> return $ VApp (VDef n) vl
                                       Just v2 -> return v2
        _ -> return $ VApp (VDef n) vl   

---- Pattern Matching ----

matchClauses :: [Clause] -> [Val] -> TypeCheck (Maybe Val)
matchClauses cl cll = loop cl
    where loop [] = return Nothing
          loop  (Clause pl rhs : cl2) = 
              do x <- matchClause [] pl rhs cll 
                 case x of
                   Nothing -> loop cl2
                   Just v -> return $ Just v

matchClause :: Env -> [Pattern] -> Expr -> [Val] -> TypeCheck (Maybe Val)
matchClause env [] rhs vl = do v <- whnf env rhs
                               v2 <- app v vl
                               return $ Just v2
matchClause env (p:pl) rhs (v:vl) =
  do m <- match env p v 
     case m of
       Just env' ->
         matchClause env' pl rhs vl
       Nothing -> return Nothing
-- too few arguments
matchClause _ _ _ [] = return Nothing

match :: Env -> Pattern -> Val-> TypeCheck (Maybe Env)
match env p v0 = --trace (show env ++ show v0) $ 
  do 
    -- force against coinductive constructor pattern
    v <- case p of 
           (ConP CoInd _ _) -> force v0
           _ -> return v0
    case (p,v) of
      (DotP _,_) -> return $ Just env
      (VarP x,_) -> return $ Just (update env x v)
      (ConP _ x [],VCon _ y) | x== y -> return $ Just env
      (ConP _ x pl,VApp (VCon _ y) vl) | x == y -> matchList env pl vl
      (SuccP p',VInfty) -> match env p' VInfty 
      (SuccP p',VSucc v') -> match env p' v'
      _ -> return Nothing

matchList :: Env -> [Pattern] -> [Val] -> TypeCheck (Maybe Env)
matchList env [] [] = return $ Just env
matchList env (p:pl) (v:vl) = do m <- match env p v 
                                 case m of 
                                   Just env' -> matchList env' pl vl
                                   Nothing -> return Nothing
matchList _ _ _ = return Nothing

--------------------

-- check that a does not occur in tv
-- a may be a "atomic value" i.e not pi , lam , app , or succ 
nocc :: Int -> Val -> TVal -> TypeCheck Bool
nocc k a tv = -- trace ("noccRecArg " ++ show tv) 
     case tv of
         VPi x av env b -> 
             do no <- nocc k a av
                case no of 
                  True -> do
                      bv <- whnf (update env x (VGen k)) b
                      nocc (k+1) a bv
                  False  -> return False
         VLam x env b -> do
                 bv <- whnf (update env x (VGen k)) b
                 nocc (k+1) a bv
         VSucc v -> nocc k a v
         VApp v1 vl -> do n <- nocc k a v1
                          nl <- mapM (nocc k a) vl
                          return $ n && and nl
         _ -> return $ a /= tv



module Value where

import Abstract

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Debug.Trace

{-
data Clos = Clos Env Expr 
          deriving (Eq)
-}

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
           | VClos Env Expr 
             deriving (Eq)

type Clos = Val 
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
      showVals (v:vs) = showVal v ++ (if null vs then "" else " " ++ showVals vs)   
showVal (VCon _ n) = n
showVal (VDef n) = n
showVal (VGen k) = show k
showVal (VClos env e) = showEnv env ++ prettyExpr e

showEnv :: Env -> String
showEnv [] = ""
showEnv x = "{" ++ showEnv' x ++ "} " where
showEnv' [] = []
showEnv' ((n,v):env) = "(" ++ n ++ " = " ++ showVal v ++ ")" ++ showEnv' env 


runPrettyVal :: Env -> Signature -> TypeCheck a -> Either TraceError (a,Signature)
runPrettyVal env sig tc = runIdentity (runErrorT (runStateT (runReaderT tc env) sig)) 
 
doPrettyVal sig v = runPrettyVal emptyEnv sig (prettyVal v)

prettyVal :: Val  -> TypeCheck String
prettyVal v = do v' <- whnf v
                 prettyVal' v'
    where
      prettyVal' :: Val -> TypeCheck String
      prettyVal' VSet = return "Set"
      prettyVal' VSize = return "Size"
      prettyVal' VInfty = return "#"
      prettyVal' (VSucc v) = do pv <- prettyVal v
                                return $ "($" ++ pv ++ ")" 
      -- don't whnf arguments of a coinductive constructor
      prettyVal' (VApp v@(VCon CoInd _) vl) = do pv <- prettyVal v
                                                 pvl <- mapM prettyVal' vl
                                                 let pvl' = showStrs pvl
                                                 return $ "(" ++ pv ++ " " ++ pvl' ++ ")"
      prettyVal' (VApp v vl) = do pv <- prettyVal v
                                  pvl <- mapM prettyVal vl
                                  let pvl' = showStrs pvl
                                  return $ "(" ++ pv ++ " " ++ pvl' ++ ")"
      prettyVal' (VCon _ n) = return n
      prettyVal' (VDef n) = return n
      prettyVal' (VGen k) = return $ show k
      prettyVal' (VClos env e) = return $ showEnv env ++ prettyExpr e
      
      showStrs :: [String] -> String
      showStrs [] = ""
      showStrs (s:sl) = s ++ (if null sl then "" else " " ++ showStrs sl)

----

type Env = [(Name,Val)]

emptyEnv = []

update :: Env -> Name -> Val -> Env
update env "" v = env
update env n v = (n,v):env

lookupEnv :: Env -> Name -> Val
lookupEnv [] n = error $ "lookup env error " ++ n 
lookupEnv ((x,v):xs) n = if x == n then v else lookupEnv xs n




---


type Signature = Map.Map Name SigDef

data Sized = Sized | NotSized 
             deriving (Eq,Show)



data SigDef = FunSig Co TVal Int [Clause] --type , co , arity , clauses
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


whnf :: Val -> TypeCheck Val
whnf v = 
    --trace ("whnf " ++ show v) $
    case v of
      (VClos _ (Lam _ _)) -> return v
      (VClos _ (Pi _ _ _)) -> return v
      VClos env e ->
            case e of
               Set -> return  VSet
               Infty -> return VInfty
               Succ e -> do v <- whnf (VClos env e)
                            vsucc v
               Size -> return VSize
               Con co n -> return $ VCon co n
               App e1 e2 -> do v1 <- whnf (VClos env e1)
                               app v1 (map (VClos env) e2)
               Def n -> return $ VDef n
               Const n -> do sig <- get 
                             let (ConstSig _ e) = lookupSig n sig
                             whnf (VClos [] e)
               Var y -> do let v' = lookupEnv env y
                           whnf v'
      _ -> return v

vsucc :: Val -> TypeCheck Val
vsucc v = do v <- whnf v 
             case v of
               VInfty -> return VInfty
               VSucc x' -> do x'' <- vsucc x'
                              return $ VSucc x''
               _ -> return $ VSucc v

vclos :: Env -> Expr -> TypeCheck Val
vclos env e = return $ VClos env e


app :: Val -> [Clos] -> TypeCheck Val
app u [] = return $ u
app u v = do  
         case (u,v) of
            (VApp u2 v2,_) -> app u2 (v2 ++ v)
            (VClos env (Lam x e),(v:vl))  -> do v' <- whnf (VClos (update env x v) e)
                                                app v' vl
            (VClos env (Pi x _ e),(v:vl)) -> do v' <- whnf (VClos (update env x v) e)
                                                app v' vl
            (VDef n,_) -> appDef n v
            _ -> return $ VApp u v

-- unroll a corecursive definition once
force :: Val -> TypeCheck Val
force v@ (VDef n) = --trace ("force " ++ show v) $
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity == 0 -> do m <- matchClauses n cl []
                                                      case m of
                                                        Just v' -> return v'
                                                        _ -> return v
         _ -> return v
force v@(VApp (VDef n) vl) = --trace ("force " ++ show v) $
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= (length vl) -> do m <- matchClauses n cl vl
                                                                case m of 
                                                                  Just v' -> return v'
                                                                  _ -> return v
         _ -> return v
force v = return v


canForce :: Val -> TypeCheck Bool
canForce v@ (VDef n) =  
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= 0 -> do m <- matchClauses n cl []
                                                      case m of
                                                        Nothing -> return False
                                                        Just v2 -> return True
         _ -> return False 
canForce v@(VApp (VDef n) vl) =
    do sig <- get
       case lookupSig n sig of
         (FunSig CoInd t arity cl) | arity <= (length vl) -> do m <- matchClauses n cl vl
                                                                case m of 
                                                                  Nothing -> return False
                                                                  Just v2 -> return True
         _ -> return False
canForce v = return False


appDef :: Name -> [Clos] -> TypeCheck Val
appDef n vl = --trace ("appDef " ++ n) $ 
    do
      sig <- get
      case lookupSig n sig of
        (FunSig Ind t arity cl) -> do m <- matchClauses n cl vl
                                      case m of
                                        Nothing -> return $ VApp (VDef n) vl
                                        Just v2 -> return v2
        _ -> case vl of 
               [] -> return $ VDef n  
               _ -> return $ VApp (VDef n) vl   

---- Pattern Matching ----

matches :: Name -> Env -> Pattern -> Val -> TypeCheck (Maybe Env)
matches n env p v = --trace (n ++ " :" ++ show p ++ show v) $ 
  do 
    v <- whnf v
    -- force against coinductive constructor pattern
    v <- case p of
           ConP CoInd _ _ -> do v' <- force v
                                return $ v' 
           _ -> return v
    case (p,v) of
      (DotP _,_) -> return $ Just env
      (VarP x,_) -> return $ Just (update env x v)
      (ConP _ x [],VCon _ y) | x== y -> return $ Just env
      (ConP _ x pl,VApp (VCon _ y) vl) | x == y -> matchesList n env pl vl
      (_,VInfty) -> matchesInfty env p 
      (SuccP p',VSucc v') -> matches n env p' v'
      _ -> return Nothing

matchesInfty :: Env -> Pattern -> TypeCheck (Maybe Env)
matchesInfty env p = 
    case p of
        VarP x -> return $ Just $ update env x VInfty
        SuccP p' -> matchesInfty env p'
        _ -> return Nothing

matchesList :: Name -> Env -> [Pattern] -> [Val] -> TypeCheck (Maybe Env)
matchesList n env [] [] = return $ Just env
matchesList n env (p:pl) (v:vl) = do m <- matches n env p v 
                                     case m of 
                                       Just env' -> matchesList n env' pl vl
                                       Nothing -> return Nothing
matchesList _ _ _ _ = return Nothing

matchClauses :: Name -> [Clause] -> [Clos] -> TypeCheck (Maybe Val)
matchClauses n cl vl = loop cl
    where loop [] = return Nothing
          loop  (Clause pl rhs : cl2) = 
              do x <- matchClause n [] pl rhs vl 
                 case x of
                   Nothing -> loop cl2
                   Just v -> return $ Just v

matchClause :: Name -> Env -> [Pattern] -> Expr -> [Clos] -> TypeCheck (Maybe Val)
matchClause n env [] rhs vl = do v <- whnf (VClos env rhs)
                                 v2 <- app v vl
                                 return $ Just v2
matchClause n env (p:pl) rhs (v:vl) =
 do  m <- matches n env p v 
     case m of
       Just env ->
         matchClause n env pl rhs vl
       Nothing -> return Nothing
-- too few arguments
matchClause _ _ _ _ [] = return Nothing

module ScopeChecker (scopeCheck) where

import Abstract

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

--scope checker
-- check if all identifiers are "in scope"
-- and
-- replaces Ident with Con, Def , Const or Var  
-- replaces IdentP with ConP or VarP in patterns

--------------------


--local environment
type Context = [Name] 

emptyCtx :: Context
emptyCtx = []


lookupCtx :: Name -> Context -> Bool
lookupCtx n [] = False
lookupCtx n (x:xs) = if (x == n) then True else lookupCtx n xs 

addCtx :: Name -> Context -> Context
addCtx n ctx = ctx ++ [n]

addCtxs :: [Name] -> Context -> Context
addCtxs nl ctx = ctx ++ nl

--global signature

data Kind = DataK | ConK | FunK | ConstK

type Sig = [(Name,Kind)] -- we record if a name is a constructor name or something else

emptySig :: Sig
emptySig= []

lookupSig :: Name -> Sig -> Maybe Kind
lookupSig n [] = Nothing
lookupSig n ((x,k):xs) = if (x == n) then Just k else lookupSig n xs

addSig :: Name -> Kind -> Sig -> Sig
addSig n k sig = sig ++ [(n,k)]

-- reader monad for local environment (only used in expresssions and patterns)

-- state monad for global signature
type ScopeCheck a = ReaderT Context (StateT Sig Identity) a

runScopeCheck :: Context -> Sig -> ScopeCheck a -> (a,Sig)
runScopeCheck ctx sig sc = runIdentity (runStateT (runReaderT sc ctx) sig)  

scopeCheck :: [Declaration] -> [Declaration]
scopeCheck dl = fst (runScopeCheck emptyCtx emptySig (scopeCheckDecls dl))

---------


scopeCheckDecls :: [Declaration] -> ScopeCheck [Declaration]
scopeCheckDecls = mapM scopeCheckDeclaration 

scopeCheckDeclaration :: Declaration -> ScopeCheck Declaration
scopeCheckDeclaration (Declaration tsl dl) = do tsl' <- mapM scopeCheckTypeSig (zip tsl dl)
                                                dl' <- mapM scopeCheckDefinition dl  
                                                return $ Declaration tsl' dl'

scopeCheckTypeSig :: (TypeSig,Definition) -> ScopeCheck TypeSig 
scopeCheckTypeSig (a@(TypeSig n e),def) = do sig <- get
                                             case (lookupSig n sig) of
                                               Just _ -> errorAlreadyInSignature  a n  
                                               Nothing -> do e' <- scopeCheckExpr e 
                                                             put (addSig n kind sig)
                                                             return $ TypeSig n e'
    where kind = case def of 
                   (DataDef _ _ _) -> DataK
                   (FunDef _ _) -> FunK
                   (ConstDef _) -> ConstK

                           

scopeCheckDefinition :: Definition -> ScopeCheck Definition
scopeCheckDefinition (DataDef co tl cs) = do tl' <- scopeCheckTelescope tl
                                             let names = collectTelescopeNames tl' 
                                             cs' <- local (addCtxs names) (mapM scopeCheckConstructor cs) 
                                             return $ DataDef co tl' cs'
scopeCheckDefinition (FunDef co cls) = do cls' <- mapM scopeCheckClause cls
                                          return $ FunDef co cls'
scopeCheckDefinition (ConstDef e) = do e' <- scopeCheckExpr e
                                       return $ ConstDef e'

collectTelescopeNames :: Telescope -> [Name]
collectTelescopeNames = map ( \(TBind n e) -> n )

scopeCheckConstructor :: Constructor -> ScopeCheck Constructor
scopeCheckConstructor a@(TypeSig n e) = 
    do sig <- get
       case (lookupSig n sig) of
         Just _ -> errorAlreadyInSignature a n
         Nothing -> do e' <- scopeCheckExpr e 
                       put (addSig n ConK sig)
                       return $ TypeSig n e'

scopeCheckTelescope :: Telescope -> ScopeCheck Telescope
scopeCheckTelescope [] = return []
scopeCheckTelescope a@((TBind n t):xs) = 
    do ctx <- ask
       sig <- get
       case (lookupSig n sig) of
         Just _ ->  errorAlreadyInSignature (show t) n
         Nothing -> case (lookupCtx n ctx) of 
                      True  -> errorAlreadyInContext a n
                      False -> do t' <- scopeCheckExpr t
                                  xs' <- local (addCtx n) (scopeCheckTelescope xs)
                                  return $ (TBind n t') : xs'
                      

scopeCheckExpr :: Expr -> ScopeCheck Expr
scopeCheckExpr e = 
    case e of 
      Set -> return Set
      Size -> return Size
      Succ e1 -> do e1' <- scopeCheckExpr e1
                    return $ Succ e1'
      Infty -> return Infty
      App e1 el -> do e1' <- scopeCheckExpr e1
                      el' <- mapM scopeCheckExpr el
                      return $ App e1' el'
      
      Fun e1 e2 -> do e1' <- scopeCheckExpr e1
                      e2' <- scopeCheckExpr e2
                      return $ Fun e1' e2'
      -- interesting cases
      Pi (TBind n t) e1 -> do ctx <- ask
                              sig <- get
                              case (lookupSig n sig) of
                                Just _ ->  errorAlreadyInSignature e n 
                                Nothing -> case (lookupCtx n ctx) of 
                                             True  -> errorAlreadyInContext e n
                                             False -> do t' <- scopeCheckExpr t
                                                         e1' <- local (addCtx n) (scopeCheckExpr e1)
                                                         return $ Pi (TBind n t') e1'
      Lam n e1 -> do ctx <- ask
                     sig <- get
                     case (lookupSig n sig) of
                       Just _ ->  errorAlreadyInSignature e n
                       Nothing -> case (lookupCtx n ctx) of 
                                    True  -> errorAlreadyInContext e n
                                    False -> do e1' <- local (addCtx n) (scopeCheckExpr e1)
                                                return $ Lam n e1'
      Ident n -> do ctx <- ask
                    sig <- get
                    case (lookupSig n sig) of
                      Just k -> case k of
                                  ConK -> return $ Con n
                                  ConstK -> return $ Const n
                                  _ -> return $ Def n
                      Nothing -> case (lookupCtx n ctx) of
                                   True -> return $ (Var n)
                                   False -> errorIdentifierUndefined n 

-- first check the Patterns (replacing IdentP with VarP or ConP)
-- and then check the RHS with all the VarP names in the context
scopeCheckClause :: Clause -> ScopeCheck Clause
scopeCheckClause (Clause (LHS pl) rhs) = 
    do pl' <- mapM scopeCheckPattern pl
       let names = collectVarPNames pl'
       rhs' <- local (addCtxs names) (scopeCheckRHS rhs)
       return $ Clause (LHS pl') rhs'

-- collect all variable names, checks if pattern is linear
collectVarPNames :: [Pattern] -> [Name]
collectVarPNames pl = cvp pl [] where
    cvp [] acc = acc
    cvp (p:pl) acc = case p of
                       WildP -> acc
                       AbsurdP -> acc
                       SuccP p2 -> cvp (p2:pl) acc
                       ConP n pl2 -> cvp (pl2++pl) acc
                       VarP n -> if (elem n acc) then 
                                     error $ "not a linear pattern: " ++ n ++ " already used"
                                 else 
                                     cvp pl (n:acc)

scopeCheckPattern :: Pattern -> ScopeCheck Pattern
scopeCheckPattern p = 
    case p of
      IdentP n -> do sig <- get
                     ctx <- ask
                     case (lookupSig n sig) of
                       (Just ConK) -> return $ ConP n [] -- assumed to be a nullary constructor
                       (Just _) -> errorPatternNotConstructor n
                       Nothing -> return $ VarP n
      SuccP p -> do p' <- scopeCheckPattern p
                    return $ SuccP p'
      WildP -> return WildP
      AbsurdP -> return AbsurdP
      ConP n pl -> do sig <- get
                      case (lookupSig n sig) of
                        (Just ConK) -> do pl' <- mapM scopeCheckPattern pl 
                                          return $ ConP n pl'
                        (Just _) -> errorPatternNotConstructor n 
                        Nothing -> errorPatternNotConstructor n 

scopeCheckRHS :: RHS -> ScopeCheck RHS
scopeCheckRHS AbsurdRHS = return $ AbsurdRHS
scopeCheckRHS (RHS e) = do e' <- scopeCheckExpr e 
                           return $ RHS e'


errorAlreadyInSignature s n = error $ "Scope Error at " ++ show s  ++ ": Identifier " ++ n ++ " already in signature"

errorAlreadyInContext s n = error $ "Scope Error at " ++ show s ++ ": Identifier " ++ n ++ " already in context"

errorPatternNotConstructor n = error $ "Scope Error in pattern: " ++ show n ++ " not a constructor"

errorIdentifierUndefined n = error $ "Scope Error: Identifier " ++ n ++ " undefined"
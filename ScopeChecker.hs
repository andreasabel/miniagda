module ScopeChecker (scopeCheck) where

import Abstract

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

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
type ScopeCheck a = ReaderT Context (StateT Sig (ErrorT String Identity)) a

runScopeCheck :: Context -> Sig -> ScopeCheck a -> Either String (a,Sig)
runScopeCheck ctx sig sc = runIdentity (runErrorT (runStateT (runReaderT sc ctx) sig))  

scopeCheck :: [Declaration] -> Either String ([Declaration],Sig)
scopeCheck dl = runScopeCheck emptyCtx emptySig (scopeCheckDecls dl)

---------


scopeCheckDecls :: [Declaration] -> ScopeCheck [Declaration]
scopeCheckDecls = mapM scopeCheckDeclaration
               
scopeCheckDeclaration :: Declaration -> ScopeCheck Declaration
scopeCheckDeclaration (DataDecl n co tel t cs) = do let tt = teleToType tel t
                                                    (TypeSig _ tt') <- scopeCheckTypeSig DataK (TypeSig n tt)
                                                    let (tel',t') = 
                                                            splitTeleType (length tel) ([],tt') -- get original tele back
                                                    let names = collectTelescopeNames tel
                                                    cs' <- local (addCtxs names) (mapM scopeCheckConstructor cs)
                                                    return $ DataDecl n co tel' t' cs'
scopeCheckDeclaration (FunDecl co funs) = do tsl' <- mapM (scopeCheckTypeSig FunK . fst ) funs  
                                             cll' <- mapM ((mapM scopeCheckClause) . snd ) funs
                                             return $ FunDecl co (zip tsl' cll') 
scopeCheckDeclaration (ConstDecl ts e) =  do ts' <- scopeCheckTypeSig ConstK ts
                                             e' <- scopeCheckExpr e
                                             return $ ConstDecl ts' e'



scopeCheckTypeSig :: Kind -> TypeSig -> ScopeCheck TypeSig 
scopeCheckTypeSig kind a@(TypeSig n e) = 
    do sig <- get
       case (lookupSig n sig) of
         Just _ -> errorAlreadyInSignature a n
         Nothing -> do e' <- scopeCheckExpr e 
                       put (addSig n kind sig)
                       return $ TypeSig n e'         

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


scopeCheckClause :: Clause -> ScopeCheck Clause
scopeCheckClause (Clause (LHS pl) rhs) = 
    do pl' <- mapM scopeCheckPattern pl
       names <- collectVarPNames pl'
       pl'' <- local (addCtxs names) (mapM replaceIdentDotPattern pl') 
       rhs' <- local (addCtxs names) (scopeCheckRHS rhs)
       return $ Clause (LHS pl'') rhs'



-- collect all variable names, checks if pattern is linear
-- check dot patterns 
collectVarPNames :: [Pattern] -> ScopeCheck [Name]
collectVarPNames pl = cvp pl [] where
    cvp [] acc = return acc
    cvp (p:pl) acc = case p of
                       WildP -> cvp pl acc
                       AbsurdP -> cvp pl acc
                       SuccP p2 -> cvp (p2:pl) acc
                       ConP n pl2 -> cvp (pl2++pl) acc
                       VarP n ->  if (elem n acc) then 
                                      cvp pl acc
                                  else 
                                      cvp pl (n:acc)
                       DotP e -> --do local (addCtxs acc) $ scopeCheckExpr e 
                            cvp pl acc

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
      ConP n pl -> do sig <- get
                      case (lookupSig n sig) of
                        (Just ConK) -> do pl' <- mapM scopeCheckPattern pl 
                                          return $ ConP n pl'
                        (Just _) -> errorPatternNotConstructor n 
                        Nothing -> errorPatternNotConstructor n 
      _ -> return p -- dot patterns checked later
    
replaceIdentDotPattern :: Pattern -> ScopeCheck Pattern
replaceIdentDotPattern p  = 
    case p of 
      DotP e -> do e' <- scopeCheckExpr e
                   return $ DotP e'
      SuccP p2 -> do p2' <- replaceIdentDotPattern p2
                     return $ SuccP p2'
      (ConP n pl) -> do pl' <- mapM replaceIdentDotPattern pl
                        return $ ConP n pl'
      _ -> return p 



scopeCheckRHS :: RHS -> ScopeCheck RHS
scopeCheckRHS AbsurdRHS = return $ AbsurdRHS
scopeCheckRHS (RHS e) = do e' <- scopeCheckExpr e 
                           return $ RHS e'


errorAlreadyInSignature s n = throwError $ "Scope Error at " ++ show s  ++ ": Identifier " ++ n ++ " already in signature"

errorAlreadyInContext s n = throwError $ "Scope Error at " ++ show s ++ ": Identifier " ++ n ++ " already in context"

errorPatternNotConstructor n = throwError $ "Scope Error in pattern: " ++ show n ++ " not a constructor"

errorIdentifierUndefined n = throwError $ "Scope Error: Identifier " ++ n ++ " undefined"
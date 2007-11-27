module ScopeChecker (scopeCheck) where

import qualified Abstract as A
import qualified Concrete as C

import Abstract (Name,Co,Pos)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.List

import Debug.Trace

--scope checker
-- check that all identifiers are in scope and global identifiers are only used once

-- replaces Ident with Con, Def , Const or Var  

-- replaces IdentP with ConP or VarP in patterns

-- check pattern length is equal in each clause

-- group mutual declarations

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

data Kind = DataK | ConK Co | FunK | ConstK

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

scopeCheck :: [C.Declaration] -> Either String ([A.Declaration],Sig)
scopeCheck dl = runScopeCheck emptyCtx emptySig (scopeCheckDecls dl)

---------


scopeCheckDecls :: [C.Declaration] -> ScopeCheck [A.Declaration]
scopeCheckDecls = mapM scopeCheckDeclaration 
                       

scopeCheckDeclaration :: C.Declaration -> ScopeCheck A.Declaration
scopeCheckDeclaration d@(C.DataDecl _ _ _ _ _ _) = scopeCheckDataDecl d 
scopeCheckDeclaration d@(C.FunDecl _ _ _ ) =  scopeCheckFunDecls [d]
                                               
scopeCheckDeclaration (C.ConstDecl b ts e) = do e' <- scopeCheckExpr e
                                                ts' <- scopeCheckTypeSig ts
                                                addTypeSig ConstK ts 
                                                return $ A.ConstDecl b ts' e'

-- we support
-- - mutual (co)funs
-- - mutual (co)data

scopeCheckDeclaration (C.MutualDecl []) = throwError "mutual combination not supported"
scopeCheckDeclaration (C.MutualDecl l@(C.FunDecl _ _ _:xl)) = scopeCheckFunDecls l 
scopeCheckDeclaration (C.MutualDecl _) = throwError "mutual combination not supported"
                                
scopeCheckDataDecl :: C.Declaration -> ScopeCheck A.Declaration
scopeCheckDataDecl decl@(C.DataDecl n sz co tel t cs) = do
      let pos = posTelescope tel
      let tt = C.teleToType tel t
      (A.TypeSig _ tt') <- scopeCheckTypeSig (C.TypeSig n tt)
      let (tel',t') = A.splitTeleType (length tel) ([],tt')                
      addDecl decl
      let names = collectTelescopeNames tel
      cs' <- local (addCtxs names) (mapM (scopeCheckConstructor co) cs ) 
      return $ A.DataDecl n sz co pos tel' t' cs'
      
checkFunMutual [] = return ()
checkFunMutual (C.FunDecl _ _ _:xl) = checkFunMutual xl
checkFunMutual _ = throwError "mutual combination not supported"

posTelescope :: C.Telescope -> [A.Pos]
posTelescope [] = []
posTelescope (C.TB n t:tel) = A.NSPos : posTelescope tel
posTelescope (C.PosTB n t:tel) = A.SPos :posTelescope tel

---
scopeCheckFunDecls l = 
    do checkFunMutual l
       tsl' <- mapM scopeCheckFunSig l
       mapM addDecl l
       let col = map funCoType l  
       cll' <- mapM scopeCheckFunClause l
       return $ A.FunDecl (zip3 tsl' col cll') 

funCoType :: C.Declaration -> Co
funCoType (C.FunDecl co _ _) = co
   
scopeCheckFunSig :: C.Declaration -> ScopeCheck A.TypeSig 
scopeCheckFunSig (C.FunDecl _ ts _ ) = scopeCheckTypeSig ts

scopeCheckFunClause :: C.Declaration -> ScopeCheck [A.Clause] 
scopeCheckFunClause (C.FunDecl _ _ cl) = 
    do cl' <- mapM scopeCheckClause cl
       let b = checkPatternLength cl
       case b of
          True -> return $ cl' 
          False -> throwError $ " pattern length differs"
 
scopeCheckTypeSig :: C.TypeSig -> ScopeCheck A.TypeSig 
scopeCheckTypeSig a@(C.TypeSig n t) = 
    do sig <- get
       case (lookupSig n sig) of
         Just _ -> errorAlreadyInSignature a n
         Nothing -> do t' <- scopeCheckExpr t 
                       return $ A.TypeSig n t'         

addDecl :: C.Declaration -> ScopeCheck ()
addDecl (C.DataDecl n _ _ _ _ _) = addName DataK n
addDecl (C.FunDecl _ ts _) = addTypeSig FunK ts

addTypeSig :: Kind -> C.TypeSig -> ScopeCheck ()
addTypeSig kind (C.TypeSig n _) = addName kind n

addName :: Kind -> Name -> ScopeCheck ()
addName kind n =  do sig <- get
                     put (addSig n kind sig)

collectTelescopeNames :: C.Telescope -> [Name]
collectTelescopeNames [] = []
collectTelescopeNames (C.TB n t:tel) = n : collectTelescopeNames tel
collectTelescopeNames (C.PosTB n t:tel) = n : collectTelescopeNames tel

scopeCheckConstructor :: Co -> C.Constructor -> ScopeCheck A.Constructor
scopeCheckConstructor co a@(C.TypeSig n t) = 
    do sig <- get
       case (lookupSig n sig) of
         Just _ -> errorAlreadyInSignature a n
         Nothing -> do t' <- scopeCheckExpr t 
                       put (addSig n (ConK co) sig)
                       return $ A.TypeSig n t'

                     
scopeCheckExpr :: C.Expr -> ScopeCheck A.Expr
scopeCheckExpr e = 
    case e of 
      C.Set -> return A.Set
      C.Size -> return A.Size
      C.Succ e1 -> do e1' <- scopeCheckExpr e1
                      return $ A.Succ e1'
      C.Infty -> return A.Infty
      C.App e1 el -> do e1' <- scopeCheckExpr e1
                        el' <- mapM scopeCheckExpr el
                        return $ A.App e1' el'
      C.Pi "" t e1 -> do t' <- scopeCheckExpr t -- non-dep. function type
                         e1' <- scopeCheckExpr e1
                         return $ A.Pi "" t' e1'
      -- interesting cases
      C.Pi n t e1 -> do ctx <- ask
                        sig <- get
                        case (lookupSig n sig) of
                          Just _ ->  errorAlreadyInSignature e n 
                          Nothing -> case (lookupCtx n ctx) of 
                                       True  -> errorAlreadyInContext e n
                                       False -> do t' <- scopeCheckExpr t
                                                   e1' <- local (addCtx n) (scopeCheckExpr e1)
                                                   return $ A.Pi n t' e1'
      C.Lam n e1 -> do ctx <- ask
                       sig <- get
                       case (lookupSig n sig) of
                         Just _ ->  errorAlreadyInSignature e n
                         Nothing -> case (lookupCtx n ctx) of 
                                      True  -> errorAlreadyInContext e n
                                      False -> do e1' <- local (addCtx n) (scopeCheckExpr e1)
                                                  return $ A.Lam n e1'
      C.Ident n -> do ctx <- ask
                      sig <- get
                      case (lookupSig n sig) of
                        Just k -> case k of
                                    (ConK co) -> return $ A.Con co n
                                    ConstK -> return $ A.Const n
                                    _ -> return $ A.Def n
                        Nothing -> case (lookupCtx n ctx) of
                                     True -> return $ A.Var n
                                     False -> errorIdentifierUndefined n 


scopeCheckClause :: C.Clause -> ScopeCheck A.Clause
scopeCheckClause (C.Clause pl rhs) = 
    do pl' <- mapM scopeCheckPattern pl
       names <- collectVarPNames pl'
       pl'' <- local (addCtxs names) (zipWithM scopeCheckDotPattern pl pl') 
       rhs' <- local (addCtxs names) (scopeCheckExpr rhs)
       return $ A.Clause pl'' rhs'

checkPatternLength :: [C.Clause] -> Bool
checkPatternLength [] = True
checkPatternLength (C.Clause pl _:cl) = cpl (length pl) cl
 where
   cpl k [] = True
   cpl k (C.Clause pl _ : cl) = if (length pl == k) then (cpl k cl) else False

-- collect all variable names, checks if pattern is linear
collectVarPNames :: [A.Pattern] -> ScopeCheck [Name]
collectVarPNames pl = cvp pl [] where
    cvp [] acc = return acc
    cvp (p:pl) acc = case p of
                       A.SuccP p2 -> cvp (p2:pl) acc
                       A.ConP _ n pl2 -> cvp (pl2++pl) acc
                       A.VarP "" -> cvp pl acc -- will be dot pattern
                       A.VarP n ->  if (elem n acc) then 
                                      errorPatternNotLinear n
                                    else 
                                        cvp pl (n:acc)
          

scopeCheckPattern :: C.Pattern -> ScopeCheck A.Pattern
scopeCheckPattern p = 
    case p of
      C.IdentP n -> do sig <- get
                       ctx <- ask
                       case (lookupSig n sig) of
                         (Just (ConK co)) -> return $ A.ConP co n [] -- a nullary constructor
                         (Just _) -> errorPatternNotConstructor n
                         Nothing -> return $ A.VarP n
      C.SuccP p2 -> do p2' <- scopeCheckPattern p2
                       return $ A.SuccP p2'
      C.ConP n pl -> do sig <- get
                        case (lookupSig n sig) of
                          Just (ConK co) -> 
                              do pl' <- mapM scopeCheckPattern pl 
                                 return $ A.ConP co n pl'
                          Just _ -> errorPatternNotConstructor n 
                          Nothing -> errorPatternNotConstructor n 
      C.DotP p -> return $ A.VarP "" -- dot patterns checked later
    
scopeCheckDotPattern :: C.Pattern -> A.Pattern -> ScopeCheck A.Pattern
scopeCheckDotPattern p1 p2 = 
    case p1 of 
      C.DotP e -> do e' <- scopeCheckExpr e
                     return $ A.DotP e'
      C.SuccP p1' -> do let (A.SuccP p2') = p2
                        p2'' <- scopeCheckDotPattern p1' p2'
                        return $ A.SuccP p2''
      (C.ConP _ pl) -> 
            do let (A.ConP co n pl') = p2
               pl'' <- zipWithM scopeCheckDotPattern pl pl'
               return $ A.ConP co n pl''
      _ -> return p2


errorAlreadyInSignature s n = throwError $ "Scope Error at " ++ show s  ++ ": Identifier " ++ n ++ " already in signature"

errorAlreadyInContext s n = throwError $ "Scope Error at " ++ show s ++ ": Identifier " ++ n ++ " already in context"

errorPatternNotConstructor n = throwError $ "Scope Error in pattern: " ++ n ++ " not a constructor"

errorIdentifierUndefined n = throwError $ "Scope Error: Identifier " ++ n ++ " undefined"

errorPatternNotLinear n = throwError $ "Scope Error: pattern not linear: " ++ n 

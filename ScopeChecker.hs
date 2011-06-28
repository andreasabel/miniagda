-- NOTE: insertion of polarity variables disabled here, must be done
-- in TypeChecker

{-# LANGUAGE TupleSections #-}

module ScopeChecker (scopeCheck) where

import Polarity(Pol(..)) 
import qualified Polarity as A
import Abstract (Name,Sized,mkExtRef,Co,MVar,Decoration(..),Override(..),Measure(..),adjustTopDecsM,Arity)
import qualified Abstract as A
import qualified Concrete as C

import TraceError

import Control.Applicative -- <$>
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.List as List
import Data.Maybe

import Debug.Trace

import Util

--scope checker
-- check that all identifiers are in scope and global identifiers are only used once
-- replaces Ident with Con, Def , Let or Var  
-- replaces IdentP with ConP or VarP in patterns
-- replaces Unknown by a new Meta-Variable
-- check pattern length is equal in each clause
-- group mutual declarations

--------------------


--local environment

data SCCxt = SCCxt
  { context         :: Context  -- ^ local names in scope
  , defaultPolarity :: Pol      -- ^ replacement for @Default@ polarity.
  }

initCtx :: SCCxt
initCtx = SCCxt emptyCtx A.Rec -- True -- POL VARS DISABLED!!

type Context = [Name] 

emptyCtx :: Context
emptyCtx = []

lookupCtx :: Name -> Context -> Bool
lookupCtx n [] = False
lookupCtx n (x:xs) = if (x == n) then True else lookupCtx n xs 

{- RETIRED, use addCtxs
addCtx :: Name -> SCCxt -> SCCxt
addCtx n ctx = ctx { context = (context ctx) ++ [n] }
-}

addCtxs :: [Name] -> SCCxt -> SCCxt
addCtxs nl ctx = ctx { context = nl ++ context ctx }


-- kind of identifier
data IKind = DataK 
           | ConK Co 
           | FunK Bool  -- False = inside body, True = outside body
           | ProjK      -- a record projection 
           | LetK

-- Scope check signature
type Sig = [(Name,IKind)] 

emptySig :: Sig
emptySig = []

lookupSig :: Name -> Sig -> Maybe IKind
lookupSig n [] = Nothing
lookupSig n ((x,k):xs) = if (x == n) then Just k else lookupSig n xs

{-
-- updateSig new old
updateSig :: Sig -> Sig -> Sig
updateSig = (++)
-}

-- Scope check monad
-- reader monad for local environment for variables (used in expresssions and patterns)
-- state monad for global signature

data SCState = SCState 
  { signature  :: Sig
  , nextMeta   :: MVar
  , nextPolVar :: MVar 
  }

initSt = SCState emptySig 0 0

type ScopeCheck = ReaderT SCCxt (StateT SCState (ErrorT TraceError Identity))

{- obsolete with mtl-2
instance Applicative ScopeCheck where  -- default instance for monads
  pure      = return
  mf <*> ma = mf >>= \ f -> ma >>= \ a -> pure (f a)
-}

setDefaultPolarity :: Pol -> ScopeCheck a -> ScopeCheck a
setDefaultPolarity p = local (\ sccxt -> sccxt { defaultPolarity = p })
{-
insertingPolVars :: Bool -> ScopeCheck a -> ScopeCheck a
insertingPolVars b = local (\ sccxt -> sccxt { insertPolVars = b })
-}

generalizeDec :: A.Dec -> ScopeCheck A.Dec
generalizeDec dec = 
  if (polarity dec == Default) then do
    p0 <- asks defaultPolarity
    case p0 of
      PVar{} -> nextPVar $ \ i -> 
                  return $ dec { polarity = PVar i }
      _      -> return $ dec { polarity = p0 }
   else return dec

generalizeTel :: C.Telescope -> ScopeCheck C.Telescope
generalizeTel tel = flip mapM tel $ \ tb -> do 
  dec' <- generalizeDec (C.boundDec tb)  
  return $ tb { C.boundDec = dec' }

nextMVar :: (MVar -> ScopeCheck a) -> ScopeCheck a
nextMVar f = do
  st <- get
  put $ st { nextMeta = nextMeta st + 1 }
  f (nextMeta st)

nextPVar :: (MVar -> ScopeCheck a) -> ScopeCheck a
nextPVar f = do
  st <- get
  put $ st { nextPolVar = nextPolVar st + 1 }
  f (nextPolVar st)

addSig :: Name -> IKind -> ScopeCheck ()
addSig n k = do
  st <- get
  put $ st { signature = (n,k) : signature st }  


runScopeCheck :: SCCxt -> SCState -> ScopeCheck a -> Either TraceError (a,SCState)
runScopeCheck ctx st sc = runIdentity (runErrorT (runStateT (runReaderT sc ctx) st))  

scopeCheck :: [C.Declaration] -> Either TraceError ([A.Declaration],SCState)
scopeCheck dl = runScopeCheck initCtx initSt (scopeCheckDecls dl)

---------

{-
scopeCheckDecls :: [C.Declaration] -> ScopeCheck [A.Declaration]
scopeCheckDecls ds = do
   ass <- mapM scopeCheckDeclaration ds
   return $ concat ass 
-}

scopeCheckDecls :: [C.Declaration] -> ScopeCheck [A.Declaration]
scopeCheckDecls = mapM scopeCheckDeclaration 
                       

scopeCheckDeclaration :: C.Declaration -> ScopeCheck A.Declaration

scopeCheckDeclaration (C.OverrideDecl Check ds) = do
  st <- get
  as <- scopeCheckDecls ds -- declarations need to scope check
  put st                   -- but then forget their effect: restore old state
  return $ A.OverrideDecl Check as

scopeCheckDeclaration (C.OverrideDecl Fail ds) = do
  st <- get
  as <- scopeCheckDecls ds
               `catchError` (const $ return [])  --on error discard block 
  put st
  return $ A.OverrideDecl Fail as
{-
scopeCheckDeclaration (C.OverrideDecl Fail ds) = do
  st <- get
  (as,st') <- (do as  <- scopeCheckDecls ds
                  st' <- get
                  return (as,st'))
               `catchError` (const $ return ([],st))  --on error discard block 
  put st'
  return $ A.OverrideDecl Fail as
-}
scopeCheckDeclaration (C.OverrideDecl override ds) = do -- TrustMe,Impredicative
  as <- scopeCheckDecls ds
  return $ A.OverrideDecl override as

scopeCheckDeclaration (C.RecordDecl n tel t c fields) =
  scopeCheckRecordDecl n tel t c fields

scopeCheckDeclaration d@(C.DataDecl{}) = 
  scopeCheckDataDecl d -- >>= return . (:[])

scopeCheckDeclaration d@(C.FunDecl co _ _) = 
  scopeCheckFunDecls co [d] -- >>= return . (:[])
                                               
scopeCheckDeclaration (C.LetDecl b ts e) = do 
  ts' <- scopeCheckTypeSig ts
  e'  <- setDefaultPolarity A.Rec $ scopeCheckExpr e
  addTypeSig LetK ts 
  return $ A.LetDecl b ts' e'

-- we support
-- - mutual (co)funs
-- - mutual (co)data

scopeCheckDeclaration (C.MutualDecl []) = throwErrorMsg "empty mutual block"
scopeCheckDeclaration (C.MutualDecl l@(C.DataDecl{}:xl)) = 
  scopeCheckMutual l
scopeCheckDeclaration (C.MutualDecl l@(C.FunDecl  co _ _:xl)) = 
  scopeCheckFunDecls co l  -- >>= return . (:[])
scopeCheckDeclaration (C.MutualDecl _) = throwErrorMsg "mutual combination not supported"

{- scopeCheck Mutual block 
first check signatures
then bodies
-}
scopeCheckMutual :: [C.Declaration] -> ScopeCheck A.Declaration
scopeCheckMutual ds0 = do
  -- flatten nested mutual blocks and override decls
  ds <- mutualFlattenDecls ds0
  -- extract, check, and add type signatures
  let ktsigs = map mutualGetTypeSig ds
  (mmm, tsigs') <- unzip <$> mapM checkAndAddTypeSig ktsigs
  -- check that all functions are unmeasured or have a same length measure
  let mll = compressMaybes mmm
  let measured = null mll || isJust (head mll)
  let ok = null mll || all ((head mll)==) (tail mll)
  when (not ok) $ fail $ "in a mutual function block, either all functions must be without measure or have a measure of the same length"
  -- switch to internal fun ids
  let funNames = map (\ (_, C.TypeSig n _) -> n) $ filter aux ktsigs where
                   aux (FunK _, _) = True 
                   aux _ = False
  mapM_ (addName (FunK False)) funNames
  -- check bodies of declarations
  ds' <- mapM (setDefaultPolarity A.Rec . checkBody) (zip tsigs' ds)
  -- switch back to external fun ids
  mapM_ (addName (FunK True)) funNames
  return $ A.MutualDecl measured ds'

checkBody :: (A.TypeSig, C.Declaration) -> ScopeCheck A.Declaration
{-
checkBody (A.TypeSig n ts, C.LetDecl b _ e) = do
  e' <- scopeCheckExpr e -- problem: n may not appear, but is already in signature !!
-}
checkBody (A.TypeSig n tt, C.DataDecl _ sz co tel _ cs fields) = 
  checkDataBody tt n sz co tel cs fields
checkBody (ts@(A.TypeSig n t), d@(C.FunDecl co tsig cls)) = do
  (ar,cls') <- scopeCheckFunClauses d
  return $ A.FunDecl co (ts,(ar,cls'))
  
mutualFlattenDecls :: [C.Declaration] -> ScopeCheck [C.Declaration]
mutualFlattenDecls ds = mapM mutualFlattenDecl ds >>= return . concat

mutualFlattenDecl :: C.Declaration -> ScopeCheck [C.Declaration]
mutualFlattenDecl (C.MutualDecl ds) = mutualFlattenDecls ds
mutualFlattenDecl (C.OverrideDecl Fail _) = fail $ "fail declaration not supported in mutual block"
mutualFlattenDecl (C.OverrideDecl o ds) = do
  ds' <- mutualFlattenDecls ds
  return $ map (\ d -> C.OverrideDecl o [d]) ds'
mutualFlattenDecl (C.LetDecl{}) = fail $ "let in mutual block not supported"
mutualFlattenDecl d = return $ [d]

-- extract type sigs of a mutual block in order, error on nested mutual
mutualGetTypeSig :: C.Declaration -> (IKind, C.TypeSig)            
mutualGetTypeSig (C.DataDecl n sz co tel t cs fields) = 
  (DataK, C.TypeSig n (C.teleToType tel t))
mutualGetTypeSig (C.FunDecl co tsig cls) = 
  (FunK True, tsig) -- fun id for use outside defining body
mutualGetTypeSig (C.LetDecl ev tsig e) = 
  (LetK, tsig)
mutualGetTypeSig (C.OverrideDecl _ [d]) = 
  mutualGetTypeSig d

{-
-- extract type sigs of a mutual block in order, error on nested mutual
mutualGetTypeSig :: C.Declaration -> ScopeCheck [(IKind, C.TypeSig)]            
mutualGetTypeSig (C.DataDecl n sz co tel t cs) = 
  [(DataK, C.TypeSig n (C.teleToType tel t))]
mutualGetTypeSig (C.FunDecl co tsig cls) = 
  [(FunK True, tsig)] -- fun id for use outside defining body
mutualGetTypeSig (C.LetDecl ev tsig e) = 
  [(LetK, tsig)]
mutualGetTypeSig (C.MutualDecl _) = fail $ "nested mutual not supported"
mutualGetTypeSig (C.OverrideDecl Fail _) = fail $ "fail declaration not supported in mutual block"
mutualGetTypeSig (C.OverrideDecl _ ds) = mutualGetTypeSigs ds

mutualGetTypeSigs ::  [C.Declaration] -> ScopeCheck [(IKind, C.TypeSig)]
mutualGetTypeSigs ds = do
  tsigss <- mapM mutualGetTypeSig ds
  return $ concat $ tsigss
-}

scopeCheckRecordDecl :: Name -> C.Telescope -> C.Type -> C.Constructor -> [Name] ->
  ScopeCheck A.Declaration
scopeCheckRecordDecl n tel t c fields = enter n $ do
  tel <- generalizeTel tel
  setDefaultPolarity A.Param $ do
    -- do not infer polarities in index arguments
    (A.TypeSig _ tt') <- scopeCheckTypeSig (C.TypeSig n $ C.teleToType tel t)
    addDecl (C.RecordDecl n tel t c fields)
    let names = collectTelescopeNames tel
    let (tel',t') = A.typeToTele' (length names) tt'                
    c' <- local (addCtxs names) (scopeCheckConstructor A.CoInd c)
    mapM_ (addName $ ProjK) fields
    return $ A.RecordDecl n tel' t' c' fields

                    
scopeCheckDataDecl :: C.Declaration -> ScopeCheck A.Declaration
scopeCheckDataDecl decl@(C.DataDecl n sz co tel0 t cs fields) = enter n $ do
  tel <- generalizeTel tel0
  setDefaultPolarity A.Param $ do
    -- do not infer polarities in index arguments
    (A.TypeSig _ tt') <- scopeCheckTypeSig (C.TypeSig n $ C.teleToType tel t)
    addDecl decl
    checkDataBody tt' n sz co tel cs fields

-- precondition: name already added to signature
checkDataBody :: A.Type -> Name -> Sized -> Co -> C.Telescope -> [C.Constructor] -> [Name] -> ScopeCheck A.Declaration
checkDataBody tt' n sz co tel cs fields = do
      let names = collectTelescopeNames tel
      let (tel',t') = A.typeToTele' (length names) tt'                
      cs' <- local (addCtxs names) (mapM (scopeCheckConstructor co) cs)
{- NO LONGER INFER DESTRUCTORS 
      -- traceM ("constructors: " ++ show cs')
--      when (t' == A.Sort A.Set && length cs' == 1) $ do
--      when (length cs' == 1) $ do  -- TOO STRICT, DOES NOT TREAT Vec right
      let cis = A.analyzeConstructors co n tel' cs'
      flip mapM_ cis $ \ ci -> when (A.cEtaExp ci) $ do
-- Add destructor names
        let fields = A.cFields ci -- A.classifyFields co n (A.typePart c)
        -- TODO Check for recursive occurrence!
        -- when (A.etaExpandable fields) $
        let destrNames =  A.destructorNames fields
        --when (not (null (destrNames))) $ 
        -- traceM ("fields: " ++ show fields)
        -- traceM ("destructors: " ++ show destrNames) 
        mapM_ (addName (FunK True)) $ destrNames -- destructors are also upped
 {- 
        let (ctel,_) = A.typeToTele (A.typePart (head cs'))
        let destrNames = map (\(_,x,_) -> x) ctel
        when (all (/= "") destrNames) $
          mapM_ (addName (FunK True)) destrNames -- destructors are also upped
-}
-}
      -- add declared destructor names
      mapM_ (addName $ FunK True) fields
      let pos = map (A.polarity . C.boundDec) tel
      return $ A.DataDecl n sz co pos tel' t' cs' fields

-- check whether all declarations in mutual block are (co)funs 
checkFunMutual :: Co -> [C.Declaration] -> ScopeCheck ()
checkFunMutual co [] = return ()
checkFunMutual co (C.FunDecl co' _ _:xl) | co == co' = checkFunMutual co xl
checkFunMutual _ _ = throwErrorMsg "mutual combination not supported"

scopeCheckFunDecls :: Co -> [C.Declaration] -> ScopeCheck A.Declaration
scopeCheckFunDecls co l = do
  -- check for uniformity of mutual block (all funs/all cofuns) 
  checkFunMutual co l
  -- check signatures and look for measures
  r <- mapM (\ (C.FunDecl _ tysig _) -> scopeCheckFunSig tysig) l
  let (ml:mll, tsl') = unzip r
  let ok = all (ml==) mll
  when (not ok) $ fail $ "in a mutual function block, either all functions must be without measure or have a measure of the same length"
  -- add names as internal ids and check bodies
  mapM (addFunDecl False) l
  arcll' <- mapM (setDefaultPolarity A.Rec . scopeCheckFunClauses) l
  -- add names as external ids
  mapM (addFunDecl True) l
  return $ A.MutualFunDecl (isJust ml) co (zip tsl' arcll') 

scopeCheckFunSig :: C.TypeSig -> ScopeCheck (Maybe Int, A.TypeSig) 
scopeCheckFunSig d@(C.TypeSig n t) = checkInSig d n $ do 
    (ml, t') <- scopeCheckFunType t 
    return (ml, A.TypeSig n t')

-- scope check type of mutual function, return length of measure (if present)
-- a fun type is a telescope followed by (maybe) a measure and a type expression
scopeCheckFunType :: C.Expr -> ScopeCheck (Maybe Int, A.Expr)
scopeCheckFunType t = 
  case t of

      -- found a measure: continue normal scope checking
      C.Pi (C.TMeasure mu) e1 -> do 
        mu' <- scopeCheckMeasure mu 
        e1' <- scopeCheckExpr e1
        return (Just $ length (measure mu'), A.Pi (A.TMeasure mu') e1')

      C.Pi (C.TBound beta) e1 -> do 
        beta'     <- scopeCheckBound beta
        (ml, e1') <- scopeCheckFunType e1
        return (ml, A.Pi (A.TBound beta') e1')
      
      C.Pi (C.TBounded dec n ltle eb) e1 -> do 
        eb'       <- setDefaultPolarity A.Rec $ scopeCheckExpr eb
        (ml, e1') <- addBind t n $ scopeCheckFunType e1 -- t is just for printing an error message
        dec'      <- generalizeDec dec
        return (ml, A.Pi (A.TBind n $ A.sizeDomain dec) $
                      A.Pi (A.TBound (A.Bound ltle (A.Measure [A.Var n])
                                                   (A.Measure [eb']))) e1')
--        return (ml, A.Pi (A.TBounded dec n ltle eb') e1')
      
      -- empty list of names in TBind means non-dep fun
      C.Pi (C.TBind dec [] t) e1 -> do 
        t'        <- setDefaultPolarity A.Rec $ scopeCheckExpr t -- non-dep. function type
        (ml, e1') <- scopeCheckFunType e1
        dec'      <- generalizeDec dec
        return (ml, A.Pi (A.TBind "" $ A.Domain t' A.defaultKind dec') e1')

      -- interesting cases
      C.Pi (C.TBind dec ns t) e1 -> do 
        t'        <- setDefaultPolarity A.Rec $ scopeCheckExpr t
        (ml, e1') <- foldr (addBind t) (scopeCheckFunType e1) ns
        dec'      <- generalizeDec dec
        return (ml, foldr (\ n e -> A.Pi (A.TBind n $ A.Domain t' A.defaultKind dec') e) e1' ns)

      t -> do
        t' <- scopeCheckExpr t
        return (Nothing, t')    -- no measure found

checkInSig :: Show d => d -> A.Name -> ScopeCheck a -> ScopeCheck a
checkInSig d n k = 
    (
    do sig <- gets signature
       case (lookupSig n sig) of
         Just _ -> errorAlreadyInSignature d n
         Nothing -> k
    ) `throwTrace` n

scopeCheckFunClauses :: C.Declaration -> ScopeCheck (Arity, [A.Clause]) 
scopeCheckFunClauses (C.FunDecl _ (C.TypeSig n _) cl) =
    (
    do cl <- mapM (scopeCheckClause (Just n)) cl
       let m = if null cl then 0 else
            List.foldl1 min $ map (length . A.clPatterns) cl
       return (A.Arity m Nothing, cl)
{-
       let b = checkPatternLength cl
       case b of
          Just m  -> return $ (A.Arity m Nothing, cl) 
          Nothing -> throwErrorMsg $ " pattern length differs"
-}
    ) `throwTrace` n

scopeCheckTypeSig :: C.TypeSig -> ScopeCheck A.TypeSig 
scopeCheckTypeSig d@(C.TypeSig n t) = checkInSig d n $ do 
    t' <- scopeCheckExpr t 
    return $ A.TypeSig n t'         

checkAndAddTypeSig :: (IKind, C.TypeSig) -> ScopeCheck (Maybe (Maybe Int), A.TypeSig) 
checkAndAddTypeSig a@(kind, ts) = do
  (mm, ts') <- 
    case kind of
      FunK _ -> do 
        (mi, ts') <- scopeCheckFunSig ts 
        return (Just mi, ts') 
      _ -> do
        ts' <- scopeCheckTypeSig ts
        return (Nothing, ts')
  uncurry addTypeSig a
  return (mm, ts')

addDecl :: C.Declaration -> ScopeCheck ()
addDecl (C.DataDecl n _ _ _ _ _ _) = addName DataK n
addDecl (C.RecordDecl n _ _ _ _)   = addName DataK n

addFunDecl :: Bool -> C.Declaration -> ScopeCheck () 
addFunDecl b (C.FunDecl _ ts _) = addTypeSig (FunK b) ts

addTypeSig :: IKind -> C.TypeSig -> ScopeCheck ()
addTypeSig kind (C.TypeSig n _) = addName kind n

addName :: IKind -> Name -> ScopeCheck ()
addName kind n =  addSig n kind

collectTelescopeNames :: C.Telescope -> [Name]
collectTelescopeNames = concat . map C.boundNames

scopeCheckConstructor :: Co -> C.Constructor -> ScopeCheck A.Constructor
scopeCheckConstructor co a@(C.TypeSig n t) = 
    do sig <- gets signature
       case (lookupSig n sig) of
         Just _ -> errorAlreadyInSignature a n
         Nothing -> do 
           t <- setDefaultPolarity A.Param $ scopeCheckExpr t 
           t <- adjustTopDecsM defaultToParam t 
           addSig n (ConK co)
           return $ A.TypeSig n t
  where defaultToParam dec = case (A.polarity dec) of
          A.Default -> return $ dec { A.polarity = A.Param }
          A.Param   -> return dec
          A.Const   -> return dec
          A.PVar{}  -> return dec
          _         -> fail $ "illegal polarity " ++ show (polarity dec) ++ " in type of constructor " ++ show a

scopeCheckExpr :: C.Expr -> ScopeCheck A.Expr
scopeCheckExpr e = 
    case e of 
      -- replace underscore by next meta-variable
      C.Unknown -> nextMVar (return . A.Meta)
{-
      C.Set -> return $ A.Sort $ A.Set $ A.Zero
      C.Type e -> scopeCheckExpr e >>= return . A.Sort . A.Set
-}
      C.Set e -> scopeCheckExpr e >>= return . A.Sort . A.Set
      C.CoSet e -> scopeCheckExpr e >>= return . A.Sort . A.CoSet
      C.Size -> return $ A.Sort (A.SortC A.Size)
      C.Succ e1 -> do e1' <- scopeCheckExpr e1
                      return $ A.Succ e1'
      C.Zero -> return A.Zero
      C.Infty -> return A.Infty
      C.Plus e1 e2 -> do 
        e1 <- scopeCheckExpr e1
        e2 <- scopeCheckExpr e2
        return $ A.Plus [e1, e2]
      C.Pair e1 e2 -> do 
        e1 <- scopeCheckExpr e1
        e2 <- scopeCheckExpr e2
        return $ A.Pair e1 e2
      C.Sing e1 et -> do e1' <- scopeCheckExpr e1
                         et' <- scopeCheckExpr et
                         return $ A.Sing e1' et'
      C.App C.Max el -> do el' <- mapM scopeCheckExpr el
                           when (length el' <  2) $ throwErrorMsg "max expects at least 2 arguments"
                           return $ A.Max el'
      C.App e1 el -> do e1' <- scopeCheckExpr e1
                        el' <- mapM scopeCheckExpr el
                        return $ foldl A.App e1' el'
      C.Case e cl -> do 
        e'  <- scopeCheckExpr e
        cl' <- mapM (scopeCheckClause Nothing) cl
        return $ A.Case e' cl'

      -- measure & bound
      -- measures can only appear in fun sigs!
      C.Pi (C.TMeasure mu) e1 -> do
        fail $ "measure not allowed in expression " ++ show e
      C.Sigma (C.TMeasure mu) e1 -> do
        fail $ "measure not allowed in expression " ++ show e
{- 
        mu' <- scopeCheckMeasure mu 
        e1' <- scopeCheckExpr e1
        return $ A.Pi (A.TMeasure mu') e1'
-}
      -- measure bound mu < mu'
      C.Pi (C.TBound beta) e1 -> do 
        beta' <- scopeCheckBound beta
        e1'   <- scopeCheckExpr e1
        return $ A.Pi (A.TBound beta') e1'

      C.Sigma (C.TBound beta) e1 -> fail $ 
        "measure bound not allowed in expression " ++ show e

      -- bounded quantification
      C.Pi (C.TBounded dec n ltle eb) e1 -> do 
        eb'   <- setDefaultPolarity A.Rec $ scopeCheckExpr eb
        e1'   <- addBind e n $ scopeCheckExpr e1 -- t is just for printing an error message
        dec'  <- generalizeDec dec
        return $ A.Pi (A.TBind n $ A.sizeDomain dec) $
                      A.Pi (A.TBound (A.Bound ltle (A.Measure [A.Var n])
                                                   (A.Measure [eb']))) e1'
--        return (A.Pi (A.TBounded dec n ltle eb') e1')
      
      -- empty list of names in TBind means non-dep fun
      C.Pi (C.TBind dec [] t) e1 -> do 
        t' <- setDefaultPolarity A.Rec $ scopeCheckExpr t -- non-dep. function type
        e1' <- scopeCheckExpr e1
        dec' <- generalizeDec dec
        return $ A.Pi (A.TBind "" $ A.Domain t' A.defaultKind dec') e1'

      -- interesting cases
      C.Pi (C.TBind dec ns t) e1 -> do 
        t'  <-  setDefaultPolarity A.Rec $ scopeCheckExpr t
        e1' <- foldr (addBind e) (scopeCheckExpr e1) ns
        dec' <- generalizeDec dec
        return $ foldr (\ n e -> A.Pi (A.TBind n $ A.Domain t' A.defaultKind dec') e) e1' ns
--        return $ A.Pi (A.TBind dec n t') e1'

      C.Lam n e1 -> do 
        e1' <- addBind e n $ scopeCheckExpr e1
        return $ A.Lam A.defaultDec n e1' -- dec. in Lam is ignored in t.c. 

      C.LLet (C.TBind dec [n] t1) e1 e2 ->  do
        t1' <- scopeCheckExpr t1
        e1' <- scopeCheckExpr e1
        e2' <- addBind e n $ scopeCheckExpr e2
        return $ A.LLet (A.TBind n $ A.Domain t1' A.defaultKind dec) e1' e2'

      C.Record rs -> do
        rs <- mapM scopeCheckRecordLine rs
        let fields = map fst rs
        if (hasDuplicate fields) then (errorDuplicateField e) else
          return $ A.Record rs

      C.Proj n -> ifM (isProjIdent n) 
                    (return $ A.Proj n) 
                    (errorUnknownProjection n)

      C.Ident n -> do 
        ctx <- asks context
        sig <- gets signature 
        case (lookupCtx n ctx) of
          True -> return $ A.Var n
          False -> case (lookupSig n sig) of
             Just k -> case k of
               (ConK co)  -> return $ A.con co n
               LetK       -> return $ A.letdef n
               -- references to recursive functions are coded differently 
               -- outside the mutual block
               FunK True  -> return $ A.mkExtRef n
               FunK False -> return $ A.fun n
               DataK      -> return $ A.dat n  
               ProjK      -> errorProjectionUsedAsExpression n
             Nothing -> errorIdentifierUndefined n 
      _ -> fail $ "NYI: scopeCheckExpr " ++ show e

scopeCheckRecordLine :: ([Name], C.Expr) -> ScopeCheck (Name, A.Expr)
scopeCheckRecordLine (n : xs, e) = do
  isProj <- isProjIdent n
  unless isProj $ errorNotAField n
  (n,) <$> scopeCheckExpr (foldr C.Lam e xs)

-- isProjIdent n = n is defined an the name of a projection
isProjIdent :: Name -> ScopeCheck Bool
isProjIdent n = do
  sig <- gets signature
  return $
    case lookupSig n sig of
      Just k -> case k of
        ProjK -> True
        _     -> False
      Nothing -> False

isProjection :: C.Expr -> ScopeCheck (Maybe Name)
isProjection (C.Ident n) = ifM (isProjIdent n) 
                            (return $ Just n) 
                            (return Nothing)
isProjection _           = return Nothing

addBind :: Show e => e -> Name -> ScopeCheck a -> ScopeCheck a
addBind e n k = do -- checkInSig e n $ do -- NO PROBLEM TO SHADOW SIG!
  ctx <- asks context 
  case (lookupCtx n ctx) of 
    True  -> errorAlreadyInContext e n
    False -> local (addCtxs [n]) k
    
scopeCheckMeasure :: A.Measure C.Expr -> ScopeCheck (A.Measure A.Expr)
scopeCheckMeasure (A.Measure es) = do
  es' <- mapM scopeCheckExpr es
  return $ A.Measure es'
    
scopeCheckBound :: A.Bound C.Expr -> ScopeCheck (A.Bound A.Expr)
scopeCheckBound (A.Bound ltle e1 e2) = do
  [e1',e2'] <- mapM scopeCheckMeasure [e1,e2]
  return $ A.Bound ltle e1' e2'

scopeCheckClause :: Maybe Name -> C.Clause -> ScopeCheck A.Clause
scopeCheckClause mname' (C.Clause mname pl mrhs) = 
    do when (mname /= mname') $ errorClauseIdentifier mname mname'
       pl' <- mapM scopeCheckPattern pl
       names <- collectVarPNames pl'
       pl'' <- local (addCtxs names) (zipWithM scopeCheckDotPattern pl pl') 
       case mrhs of
         Nothing -> return $ A.clause pl'' Nothing
         Just rhs -> do
           rhs' <- local (addCtxs names) (scopeCheckExpr rhs)
           return $ A.clause pl'' (Just rhs')

checkPatternLength :: [C.Clause] -> Maybe Int
checkPatternLength [] = Just 0 -- arity 0
checkPatternLength (C.Clause _ pl _:cl) = cpl (length pl) cl
 where
   cpl k [] = Just k
   cpl k (C.Clause _ pl _ : cl) = if (length pl == k) then (cpl k cl) else Nothing

-- collect all variable names, checks if pattern is linear
collectVarPNames :: [A.Pattern] -> ScopeCheck [Name]
collectVarPNames pl = cvp pl [] where
    cvp [] acc = return acc
    cvp (p:pl) acc = case p of
                       A.ProjP{} -> cvp pl acc
                       A.AbsurdP -> cvp pl acc
                       A.SuccP p2 -> cvp (p2:pl) acc
                       A.ConP _ n pl2 -> cvp (pl2++pl) acc
                       A.VarP "" -> cvp pl acc -- will be dot pattern
                       A.VarP n ->  if (elem n acc) then 
                                      errorPatternNotLinear n
                                    else 
                                        cvp pl (n:acc)
                       A.SizeP m n ->  
                                    if (elem n acc) then 
                                      errorPatternNotLinear n
                                    else 
                                        cvp pl (n:acc)
          

scopeCheckPattern :: C.Pattern -> ScopeCheck A.Pattern
scopeCheckPattern p = 
    case p of
      C.IdentP n -> do sig <- gets signature
                       ctx <- asks context
                       case (lookupSig n sig) of
                         (Just (ConK co)) -> return $ A.ConP pi n [] -- a nullary constructor
                                             where pi = A.PatternInfo co False
                         (Just _) -> errorPatternNotConstructor n
                         Nothing -> return $ A.VarP n
      C.SizeP m n-> do -- cxt <- ask
                       -- when (not $ lookupCtx m cxt) $ -- check this later! (dotP)
                       --   errorIdentifierUndefined m
                       sig <- gets signature
                       case (lookupSig n sig) of
                         (Just _) -> errorPatternNotConstructor n
                         Nothing -> return $ A.SizeP m n
      C.SuccP p2 -> do p2' <- scopeCheckPattern p2
                       return $ A.SuccP p2'
      C.ConP n pl -> do sig <- gets signature
                        case (lookupSig n sig) of
                          Just (ConK co) -> do
                              pl' <- mapM scopeCheckPattern pl 
                              return $ A.ConP pi n pl' where
                                pi = A.PatternInfo co False
                          Just _ -> errorPatternNotConstructor n 
                          Nothing -> errorPatternNotConstructor n 
      C.DotP e  -> do
        isProj <- isProjection e
        case isProj of 
         Just n  -> return $ A.ProjP n
         Nothing -> return $ A.VarP "" -- dot patterns checked later
      C.AbsurdP -> return $ A.AbsurdP
    
-- scopeCheckDotPattern cp ap
-- ap is the pattern produced from cp by scopeCheckPattern
-- ap = scopeCheckPattern cp
scopeCheckDotPattern :: C.Pattern -> A.Pattern -> ScopeCheck A.Pattern
scopeCheckDotPattern p1 p2 = 
    case p1 of 
      C.SizeP m n -> do cxt <- asks context
                        when (not $ lookupCtx m cxt) $
                          errorIdentifierUndefined m
                        return p2
      C.DotP e -> case p2 of
                    A.ProjP{} -> return p2
                    _         -> A.DotP <$> scopeCheckExpr e
      C.SuccP p1' -> do let (A.SuccP p2') = p2
                        p2'' <- scopeCheckDotPattern p1' p2'
                        return $ A.SuccP p2''
      (C.ConP _ pl) -> 
            do let (A.ConP co n pl') = p2
               pl'' <- zipWithM scopeCheckDotPattern pl pl'
               return $ A.ConP co n pl''
      _ -> return p2


errorAlreadyInSignature s n = throwErrorMsg $ show s  ++ ": Identifier " ++ n ++ " already in signature"

errorAlreadyInContext s n = throwErrorMsg $ show s ++ ": Identifier " ++ n ++ " already in context"

errorPatternNotConstructor n = throwErrorMsg $ "pattern " ++ n ++ " not a constructor"

errorNotAField n = throwErrorMsg $ "record field " ++ n ++ " unknown"

errorDuplicateField r = throwErrorMsg $ show r ++ " assigns a field twice"

errorUnknownProjection n = throwErrorMsg $ "projection " ++ n ++ " unknown"

errorProjectionUsedAsExpression n = throwErrorMsg $ "projection " ++ n ++ " used as expression"

errorIdentifierUndefined n = throwErrorMsg $ "Identifier " ++ n ++ " undefined"

errorPatternNotLinear n = throwErrorMsg $ "pattern not linear: " ++ n 

errorClauseIdentifier (Just n) (Just n') = throwErrorMsg $ "Expected identifier " ++ show n' ++ " as clause head, found " ++ show n
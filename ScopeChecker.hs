-- NOTE: insertion of polarity variables disabled here, must be done
-- in TypeChecker

{-# LANGUAGE TupleSections, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module ScopeChecker (scopeCheck) where

import Polarity(Pol(..)) 
import qualified Polarity as A
import Abstract (Sized,mkExtRef,Co,MVar,Decoration(..),Override(..),Measure(..),adjustTopDecsM,Arity)
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

-- * scope checker
-- check that all identifiers are in scope and global identifiers are only used once
-- replaces Ident with Con, Def, Let or Var  
-- replaces IdentP with ConP or VarP in patterns
-- replaces Unknown by a new Meta-Variable
-- check pattern length is equal in each clause
-- group mutual declarations

-- | Entry point for scope checker.
scopeCheck :: [C.Declaration] -> Either TraceError ([A.Declaration],SCState)
scopeCheck dl = runScopeCheck initCtx initSt (scopeCheckDecls dl)

-- * Local identifiers.

-- ** local environment of scope checker

data SCCxt = SCCxt
  { context         :: Context  -- ^ local names in scope
  , defaultPolarity :: Pol      -- ^ replacement for @Default@ polarity.
  }

initCtx :: SCCxt
initCtx = SCCxt emptyCtx A.Rec -- True -- POL VARS DISABLED!!

-- ** translating concrete names to abstract names

type Context = [(C.Name,A.Name)] 

emptyCtx :: Context
emptyCtx = []

lookupCtx :: C.Name -> Context -> Maybe A.Name
lookupCtx n [] = Nothing
lookupCtx n ((n',x):cxt) = if (n' == n) then Just x else lookupCtx n cxt

insertCtx :: C.Name -> Context -> (A.Name, Context)
insertCtx n ctx = (x, (n, x) : ctx)
    where x = A.fresh n

addCtx' :: C.Name -> SCCxt -> (A.Name, SCCxt)
addCtx' n ctx = (x, ctx { context = (n, x) : context ctx })
    where x = A.fresh n

addCtx :: C.Name -> SCCxt -> SCCxt
addCtx n ctx = snd $ addCtx' n ctx

{- UNUSED
addCtxs :: [C.Name] -> SCCxt -> SCCxt
addCtxs nl ctx = foldr addCtx ctx nl
-}
addContext :: Context -> SCCxt -> SCCxt
addContext delta sc = sc { context = delta ++ context sc }

-- * Global identifiers.

-- | Kind of identifier.
data IKind = DataK 
           | ConK Co 
           | FunK Bool  -- ^ @False@ = inside body, @True@ = outside body
           | ProjK      -- ^ a record projection 
           | LetK

-- | Global identifier.
data DefI = DefI { ikind :: IKind, aname :: A.Name }

-- | Scope check signature.
type Sig = [(C.Name,DefI)] 

emptySig :: Sig
emptySig = []

lookupSig :: C.Name -> Sig -> Maybe DefI
lookupSig n [] = Nothing
lookupSig n ((x,k):xs) = if (x == n) then Just k else lookupSig n xs

-- ** State of scope checker.

data SCState = SCState 
  { signature  :: Sig
  , nextMeta   :: MVar
  , nextPolVar :: MVar 
  }

initSt = SCState emptySig 0 0

-- * The scope checking monad.

-- | Scope checking monad.
--
-- Reader monad for local environment of variables (used in expresssions and patterns).
-- State monad (hidden) for global signature.
-- Error monad for reporting scope violations.
newtype ScopeCheck a = ScopeCheck { unScopeCheck :: 
  ReaderT SCCxt (StateT SCState (ErrorT TraceError Identity)) a }
  deriving (Functor, Applicative, Monad, 
    MonadReader SCCxt, MonadError TraceError)

runScopeCheck  
  :: SCCxt          -- ^ Local variable mapping.
  -> SCState        -- ^ Global identifier mapping.
  -> ScopeCheck a   -- ^ The computation. 
  -> Either TraceError (a, SCState)
runScopeCheck ctx st (ScopeCheck sc) = runIdentity $ runErrorT $ 
  runStateT (runReaderT sc ctx) st 

-- ** Local state.

{-
-- | Add a local identifier.
addBind :: Show e => e -> C.Name -> ScopeCheck a -> ScopeCheck a
addBind e n k = do -- checkInSig e n $ do -- NO PROBLEM TO SHADOW SIG!
  ctx <- asks context 
  case (lookupCtx n ctx) of 
    True  -> errorAlreadyInContext e n
    False -> local (addCtxs [n]) k
-}
-- | Add a local identifier.  
--   (Not tail recursive, since it also returns the generate id.)
addBind' :: Show e => e -> C.Name -> (A.Name -> ScopeCheck a) -> ScopeCheck (A.Name, a)
addBind' e n k = do
  ctx <- ask
  case (lookupCtx n (context ctx)) of              -- TODO: remove no shadowing 
    Just _  -> errorAlreadyInContext e n
    Nothing -> do
      let (x, ctx') = addCtx' n ctx
      a <- local (const ctx') $ k x 
      return (x, a)                

addBind :: Show e => e -> C.Name -> ScopeCheck a -> ScopeCheck (A.Name, a)
addBind e n k = addBind' e n $ const k

addBinds :: Show e => e -> [C.Name] -> ScopeCheck a -> ScopeCheck ([A.Name], a)
addBinds e ns k = foldr step start ns where
  start    = do
    a <- k
    return ([], a)
  step n k = do
    (x, (xs, a)) <- addBind e n k
    return (x:xs, a)

-- | Add local variable without checking shadowing.
addLocal :: C.Name -> (A.Name -> ScopeCheck a) -> ScopeCheck a
addLocal n k = do
  ctx <- ask
  let (x, ctx') = addCtx' n ctx
  local (const ctx') $ k x

addTel :: C.Telescope -> A.Telescope -> ScopeCheck a -> ScopeCheck a
addTel ctel atel = local (addContext nxs) -- local (\ sc -> sc { context = nxs ++ context sc })
  where nxs = reverse $ matchTels ctel atel

matchTels :: C.Telescope -> A.Telescope -> [(C.Name,A.Name)]
matchTels ctel atel = nxs
  where ns = collectTelescopeNames ctel
        xs = map A.boundName atel
        nxs = zip ns xs

-- ** Global state.

getSig :: ScopeCheck Sig
getSig = ScopeCheck $ gets signature

-- | Add a global identifier.
addName :: IKind -> C.Name -> ScopeCheck A.Name
addName k n = do
  let x = A.fresh n
  addAName k n x
  return x

-- | Add an already translated global identifier.
addAName :: IKind -> C.Name -> A.Name -> ScopeCheck ()
addAName k n x = ScopeCheck $ modify $ \ st -> 
  st { signature = (n, DefI k x) : signature st }  

{- UNUSED
addDecl :: C.Declaration -> ScopeCheck A.Name
addDecl (C.DataDecl n _ _ _ _ _ _) = addName DataK n
addDecl (C.RecordDecl n _ _ _ _)   = addName DataK n
-}
{- UNUSED
addFunDecl :: Bool -> C.Declaration -> ScopeCheck A.Name
addFunDecl b (C.FunDecl _ ts _) = addTypeSig (FunK b) ts
-}

addTypeSig :: IKind -> C.TypeSig -> A.TypeSig -> ScopeCheck () 
addTypeSig kind (C.TypeSig n _) (A.TypeSig x _) = addAName kind n x

{- UNUSED
-- | Add a global identifier.  Fail if already in signature.
addGlobal :: Show d => d -> IKind -> C.Name -> ScopeCheck A.Name
addGlobal d k n = enter n $ do
  sig <- getSig
  case lookupSig n sig of
    Just _  -> errorAlreadyInSignature d n
    Nothing -> addName k n
-}
  
-- | Create a meta variable.
nextMVar :: (MVar -> ScopeCheck a) -> ScopeCheck a
nextMVar f = ScopeCheck $ do
  st <- get
  put $ st { nextMeta = nextMeta st + 1 }
  unScopeCheck $ f (nextMeta st)

-- | Create a polarity meta variable.
nextPVar :: (MVar -> ScopeCheck a) -> ScopeCheck a
nextPVar f = ScopeCheck $ do
  st <- get
  put $ st { nextPolVar = nextPolVar st + 1 }
  unScopeCheck $ f (nextPolVar st)

-- ** Additional services of scope monad.

-- | Default polarity is context-sensitive.
setDefaultPolarity :: Pol -> ScopeCheck a -> ScopeCheck a
setDefaultPolarity p = local (\ sccxt -> sccxt { defaultPolarity = p })
{-
insertingPolVars :: Bool -> ScopeCheck a -> ScopeCheck a
insertingPolVars b = local (\ sccxt -> sccxt { insertPolVars = b })
-}

-- | Insert polarity variables for omitted polarities.
generalizeDec :: A.Dec -> ScopeCheck A.Dec
generalizeDec dec = 
  if (polarity dec == Default) then do
    p0 <- asks defaultPolarity
    case p0 of
      PVar{} -> nextPVar $ \ i -> 
                  return $ dec { polarity = PVar i }
      _      -> return $ dec { polarity = p0 }
   else return dec

-- | Insert polarity variables in telescope.
generalizeTel :: C.Telescope -> ScopeCheck C.Telescope
generalizeTel tel = flip mapM tel $ \ tb -> do 
  dec' <- generalizeDec (C.boundDec tb)  
  return $ tb { C.boundDec = dec' }

-- * Scope checking concrete syntax.
----------------------------------------------------------------------

scopeCheckDecls :: [C.Declaration] -> ScopeCheck [A.Declaration]
scopeCheckDecls = mapM scopeCheckDeclaration 
                       
scopeCheckDeclaration :: C.Declaration -> ScopeCheck A.Declaration

scopeCheckDeclaration (C.OverrideDecl Check ds) = ScopeCheck $ do
  st <- get
  as <- unScopeCheck $ scopeCheckDecls ds -- declarations need to scope check
  put st                   -- but then forget their effect: restore old state
  return $ A.OverrideDecl Check as

scopeCheckDeclaration (C.OverrideDecl Fail ds) = ScopeCheck $ do
  st <- get
  as <- unScopeCheck $ scopeCheckDecls ds
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
                                               
scopeCheckDeclaration (C.LetDecl eval ts e) = do 
  ts' <- scopeCheckTypeSig ts
  e'  <- setDefaultPolarity A.Rec $ scopeCheckExpr e
  addTypeSig LetK ts ts'
  return $ A.LetDecl eval ts' e'

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
  -- funs have been added with internal names
  -- check that all functions are unmeasured or have a same length measure
  let (ns, mll) = unzip $ compressMaybes mmm
  let measured = null mll || isJust (head mll)
  let ok = null mll || all ((head mll)==) (tail mll)
  when (not ok) $ fail $ "in a mutual function block, either all functions must be without measure or have a measure of the same length"
{-
  -- switch to internal fun ids
  let funNames = [ n | (FunK _ , A.TypeSig n _) <- ktsigs ] -- internal fun names
{- SAME W/O COMPR
  let funNames = map (\ (_, C.TypeSig n _) -> n) $ filter aux ktsigs where
                   aux (FunK _, _) = True 
                   aux _ = False
-}
  mapM_ (addName (FunK False)) funNames -- TODO
-}
  -- check bodies of declarations
  ds' <- mapM (setDefaultPolarity A.Rec . checkBody) (zip tsigs' ds)
  -- switch back to external fun ids
  let funNames = [ x | A.FunDecl _ (A.Fun _ x _ _) <- ds' ] -- external fun names
  zipWithM_ (addAName (LetK)) ns funNames
--  zipWithM_ (addAName (FunK True)) ns funNames
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
  let n' = A.mkExtName n
  return $ A.FunDecl co $ A.Fun ts n' ar cls'
 
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
  (FunK False, tsig) -- fun id for use inside defining body
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

scopeCheckRecordDecl :: C.Name -> C.Telescope -> C.Type -> C.Constructor -> [C.Name] ->
  ScopeCheck A.Declaration
scopeCheckRecordDecl n tel t c cfields = enter n $ do
  tel <- generalizeTel tel
  setDefaultPolarity A.Param $ do
    -- do not infer polarities in index arguments
    (A.TypeSig x tt') <- scopeCheckTypeSig (C.TypeSig n $ C.teleToType tel t)
    addAName DataK n x
    let names = collectTelescopeNames tel
        (tel',t') = A.typeToTele' (length names) tt'   
    c' <- addTel tel tel' $ scopeCheckConstructor A.CoInd c
    let delta = contextFromConstructors c c'
    afields <- addFields ProjK delta cfields
{-
    afields <- mapM (scopeCheckField delta) cfields
--    let afields = map A.boundName $ fst $ A.typeToTele' (length cfields) at
    mapM (uncurry $ addAName ProjK) $ zip cfields afields 
-}
    return $ A.RecordDecl x tel' t' c' afields

contextFromConstructors :: C.Constructor -> A.Constructor -> Context
contextFromConstructors (C.TypeSig _ ct) (A.TypeSig _ at) = delta
  where (ctel, _) = C.typeToTele ct
        (atel, _) = A.typeToTele at
        delta = matchTels ctel atel

scopeCheckField :: Context -> C.Name -> ScopeCheck A.Name
scopeCheckField delta n =
  case lookup n delta of
    Nothing -> errorNotAField n
    Just x  -> return $ x

addFields :: IKind -> Context -> [C.Name] -> ScopeCheck [A.Name]
addFields kind delta cfields = do 
    afields <- mapM (scopeCheckField delta) cfields
    mapM (uncurry $ addAName kind) $ zip cfields afields 
    return afields

scopeCheckDataDecl :: C.Declaration -> ScopeCheck A.Declaration
scopeCheckDataDecl decl@(C.DataDecl n sz co tel0 t cs fields) = enter n $ do
  tel <- generalizeTel tel0
  setDefaultPolarity A.Param $ do
    -- do not infer polarities in index arguments
    (A.TypeSig x tt') <- scopeCheckTypeSig (C.TypeSig n $ C.teleToType tel t)
    addAName DataK n x
    checkDataBody tt' x sz co tel cs fields

-- precondition: name already added to signature
checkDataBody :: A.Type -> A.Name -> Sized -> Co -> C.Telescope -> [C.Constructor] -> [C.Name] -> ScopeCheck A.Declaration
checkDataBody tt' n sz co tel cs fields = do
      let cnames = collectTelescopeNames tel
      let (tel',t') = A.typeToTele' (length cnames) tt'
      cs' <- addTel tel tel' (mapM (scopeCheckConstructor co) cs)
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
      let delta = concat $ map (uncurry contextFromConstructors) $ zip cs cs'
      fields <- addFields (LetK) delta fields
--      fields <- addFields (FunK True) delta fields
      -- fields <- mapM (addName $ FunK True) fields
      let pos = map (A.polarity . A.decor . A.boundDom) tel'
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
  let nxs = zipWith (\ (C.FunDecl _ (C.TypeSig n _) _) (A.TypeSig x _) -> (n,x)) l tsl'
  --let addFuns b = mapM (uncurry $ addAName $ FunK b) nxs
--  let addFuns b = mapM (\ (n,x) -> addAName (FunK b) n x) nxs
  -- addFuns False
  mapM (uncurry $ addAName $ FunK False) nxs
  arcll' <- mapM (setDefaultPolarity A.Rec . scopeCheckFunClauses) l
  -- add names as external ids
  --addFuns True
  let nxs' = map (mapPair id A.mkExtName) nxs
  mapM (uncurry $ addAName (LetK)) nxs'
--  mapM (uncurry $ addAName (FunK True)) nxs'
  return $ A.MutualFunDecl (isJust ml) co $
    zipWith3 (\ ts (_, x') (ar, cls) -> A.Fun ts x' ar cls) tsl' nxs' arcll' 
  
-- | Does not add name to signature.
scopeCheckFunSig :: C.TypeSig -> ScopeCheck (Maybe Int, A.TypeSig) 
scopeCheckFunSig d@(C.TypeSig n t) = checkInSig d n $ \ x -> do 
    (ml, t') <- scopeCheckFunType t 
    return (ml, A.TypeSig x t')

-- scope check type of mutual function, return length of measure (if present)
-- a fun type is a telescope followed by (maybe) a measure and a type expression
scopeCheckFunType :: C.Expr -> ScopeCheck (Maybe Int, A.Expr)
scopeCheckFunType t = 
  case t of

      -- found a measure: continue normal scope checking
      C.Quant A.Pi (C.TMeasure mu) e1 -> do 
        mu' <- scopeCheckMeasure mu 
        e1' <- scopeCheckExpr e1
        return (Just $ length (measure mu'), A.pi (A.TMeasure mu') e1')

      C.Quant A.Pi (C.TBound beta) e1 -> do 
        beta'     <- scopeCheckBound beta
        (ml, e1') <- scopeCheckFunType e1
        return (ml, A.pi (A.TBound beta') e1')
      
      C.Quant A.Pi (C.TBounded dec n ltle eb) e1 -> do 
        eb'            <- setDefaultPolarity A.Rec $ scopeCheckExpr eb
        (n, (ml, e1')) <- addBind t n $ scopeCheckFunType e1 -- t is just for printing an error message
        dec'      <- generalizeDec dec
        
        return (ml, A.pi (A.TBind n $ A.belowDomain dec ltle eb') $ e1')
{-
        return (ml, A.pi (A.TBind n $ A.sizeDomain dec) $
                      A.pi (A.TBound (A.Bound ltle (A.Measure [A.Var n])
                                                   (A.Measure [eb']))) e1')
 -}
--        return (ml, A.pi (A.TBounded dec n ltle eb') e1')
      
      -- empty list of names in TBind means non-dep fun
      C.Quant A.Pi (C.TBind dec [] t) e1 -> do 
        t'        <- setDefaultPolarity A.Rec $ scopeCheckExpr t -- non-dep. function type
        (ml, e1') <- scopeCheckFunType e1
        dec'      <- generalizeDec dec
        return (ml, A.funType (A.Domain t' A.defaultKind dec') e1')

      -- interesting cases
      C.Quant A.Pi (C.TBind dec ns t) e1 -> do 
        t'              <- setDefaultPolarity A.Rec $ scopeCheckExpr t
        (ns, (ml, e1')) <- addBinds t ns $ scopeCheckFunType e1
        dec'            <- generalizeDec dec
        return (ml, foldr (\ n e -> A.pi (A.TBind n $ A.Domain t' A.defaultKind dec') e) e1' ns)

      t -> (Nothing,) <$> scopeCheckExpr t -- no measure found

-- | Check whether concrete name is already in signature.  
--   If yes, fail. If no, create abstract name and continue.
checkInSig :: Show d => d -> C.Name -> (A.Name -> ScopeCheck a) -> ScopeCheck a
checkInSig d n k = enter n $ do
  sig <- getSig
  case lookupSig n sig of
    Just _  -> errorAlreadyInSignature d n
    Nothing -> k (A.fresh n)

scopeCheckFunClauses :: C.Declaration -> ScopeCheck (Arity, [A.Clause]) 
scopeCheckFunClauses (C.FunDecl _ (C.TypeSig n _) cl) = enter n $ do
  cl <- mapM (scopeCheckClause (Just n)) cl
  let m = if null cl then 0 else
       List.foldl1 min $ map (length . A.clPatterns) cl
  return (A.Arity m Nothing, cl)
{-
       let b = checkPatternLength cl
       case b of
          Just m  -> return $ (A.Arity m Nothing, cl) 
          Nothing -> throwErrorMsg $ " pattern length differs"
-}

-- | Check the type of a signature and generate abstract name.
--   Does not add abstract name to signature.
scopeCheckTypeSig :: C.TypeSig -> ScopeCheck A.TypeSig 
scopeCheckTypeSig d@(C.TypeSig n t) = checkInSig d n $ \ x -> do 
    t' <- scopeCheckExpr t 
    return $ A.TypeSig x t'         

-- | Results:
-- 
--     @Nothing@            Not a function declaration.
--
--     @Just (n, Nothing)@  Unmeasured function.
--
--     @Just (n, Just m)@   Function with measure of length m
checkAndAddTypeSig :: (IKind, C.TypeSig) -> ScopeCheck (Maybe (C.Name, Maybe Int), A.TypeSig) 
checkAndAddTypeSig (kind, ts@(C.TypeSig n _)) = do
  (mm, ts'@(A.TypeSig x _)) <- 
    case kind of
      FunK _ -> mapPair (Just . (n,)) id <$> scopeCheckFunSig ts 
{-
        do 
        (mi, ts) <- scopeCheckFunSig ts 
        return (Just mi, ts) 
-}
      _ -> (Nothing,) <$> scopeCheckTypeSig ts
  addAName kind n x  -- or: addTypeSig kind ts ts'
  return (mm, ts')

collectTelescopeNames :: C.Telescope -> [C.Name]
collectTelescopeNames = concat . map C.boundNames

scopeCheckConstructor :: Co -> C.Constructor -> ScopeCheck A.Constructor
scopeCheckConstructor co a@(C.TypeSig n t) = checkInSig a n $ \ x -> do
    t <- setDefaultPolarity A.Param $ scopeCheckExpr t 
    t <- adjustTopDecsM defaultToParam t 
    addAName (ConK co) n x
    return $ A.TypeSig x t
{-
    do sig <- getSig
       case (lookupSig n sig) of
         Just _ -> errorAlreadyInSignature a n
         Nothing -> do 
           t <- setDefaultPolarity A.Param $ scopeCheckExpr t 
           t <- adjustTopDecsM defaultToParam t 
           addName (ConK co) n
           return $ A.TypeSig n t
-}
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
      C.Quant pisig (C.TMeasure mu) e1 -> do
        fail $ "measure not allowed in expression " ++ show e
{- 
        mu' <- scopeCheckMeasure mu 
        e1' <- scopeCheckExpr e1
        return $ A.pi (A.TMeasure mu') e1'
-}
      -- measure bound mu < mu'
      C.Quant A.Pi (C.TBound beta) e1 -> do 
        beta' <- scopeCheckBound beta
        e1'   <- scopeCheckExpr e1
        return $ A.pi (A.TBound beta') e1'

      C.Quant A.Sigma (C.TBound beta) e1 -> fail $ 
        "measure bound not allowed in expression " ++ show e

      -- bounded quantification
      C.Quant pisig (C.TBounded dec n ltle eb) e1 -> do 
        eb'      <- setDefaultPolarity A.Rec $ scopeCheckExpr eb
        (n, e1') <- addBind e n $ scopeCheckExpr e1 -- e is just for printing an error message
        dec'  <- generalizeDec dec
        return $ A.Quant pisig (A.TBind n $ A.belowDomain dec ltle eb') e1'
{-
        return $ A.pi (A.TBind n $ A.sizeDomain dec) $
                      A.pi (A.TBound (A.Bound ltle (A.Measure [A.Var n])
                                                   (A.Measure [eb']))) e1'
-}
--        return (A.pi (A.TBounded dec n ltle eb') e1')
      
      -- empty list of names in TBind means non-dep fun
      C.Quant pisig (C.TBind dec [] t) e1 -> do 
        t'   <- setDefaultPolarity A.Rec $ scopeCheckExpr t -- non-dep. function type
        e1'  <- scopeCheckExpr e1
        dec' <- generalizeDec dec
        return $ A.Quant pisig (A.noBind $ A.Domain t' A.defaultKind dec') e1'

      -- interesting cases
      C.Quant pisig (C.TBind dec ns t) e1 -> do 
        t'        <-  setDefaultPolarity A.Rec $ scopeCheckExpr t
        (ns, e1') <- addBinds e ns $ scopeCheckExpr e1
        dec'      <- generalizeDec dec
        return $ foldr (\ n e -> A.Quant pisig (A.TBind n $ A.Domain t' A.defaultKind dec') e) e1' ns

      C.Lam n e1 -> do 
        (n, e1') <- addBind e n $ scopeCheckExpr e1
        return $ A.Lam A.defaultDec n e1' -- dec. in Lam is ignored in t.c. 

      C.LLet (C.TBind dec [n] t1) e1 e2 ->  do
        t1'      <- scopeCheckExpr t1
        e1'      <- scopeCheckExpr e1
        (n, e2') <- addBind e n $ scopeCheckExpr e2
        return $ A.LLet (A.TBind n $ A.Domain t1' A.defaultKind dec) e1' e2'

      C.Record rs -> do
        let fields = map fst rs
        if (hasDuplicate fields) then (errorDuplicateField e) else do
          rs <- mapM scopeCheckRecordLine rs
          return $ A.Record rs

      C.Proj n -> A.Proj <$> scopeCheckProj n
{-
      C.Proj n -> ifM (isProjIdent n) 
                    (return $ A.Proj n) 
                    (errorUnknownProjection n)
-}
      C.Ident n -> do 
        ctx <- asks context
        sig <- getSig 
        case (lookupCtx n ctx) of
          Just n -> return $ A.Var n
          Nothing -> case (lookupSig n sig) of
             Just (DefI k x) -> case k of
               (ConK co)  -> return $ A.con co x
               LetK       -> return $ A.letdef x
               -- references to recursive functions are coded differently 
               -- outside the mutual block
               FunK True  -> return $ A.fun x -- A.letdef x -- A.mkExtRef x
               FunK False -> return $ A.fun x
               DataK      -> return $ A.dat x  
               ProjK      -> errorProjectionUsedAsExpression n
             Nothing -> errorIdentifierUndefined n 
      _ -> fail $ "NYI: scopeCheckExpr " ++ show e

scopeCheckLocalVar :: C.Name -> ScopeCheck A.Name
scopeCheckLocalVar n = do
  ctx <- asks context
  case (lookupCtx n ctx) of
    Just n -> return n
    Nothing -> errorIdentifierUndefined n 

scopeCheckRecordLine :: ([C.Name], C.Expr) -> ScopeCheck (A.Name, A.Expr)
scopeCheckRecordLine (n : ns, e) = do
  x <- scopeCheckProj n
  (x,) <$> scopeCheckExpr (foldr C.Lam e ns)

{-
scopeCheckRecordLine :: ([C.Name], C.Expr) -> ScopeCheck (C.Name, A.Expr)
scopeCheckRecordLine (n : ns, e) = do
  isProj <- isProjIdent n
  unless isProj $ errorNotAField n
  (n,) <$> scopeCheckExpr (foldr C.Lam e ns)
-}

scopeCheckProj :: C.Name -> ScopeCheck A.Name
scopeCheckProj n = do
  sig <- getSig
  case lookupSig n sig of
    Just (DefI ProjK x) -> return x
    _                   -> errorNotAField n


-- isProjIdent n = n is defined and the name of a projection
isProjIdent :: C.Name -> ScopeCheck (Maybe A.Name)
isProjIdent n = do
  sig <- getSig
  return $
    case lookupSig n sig of
      Just (DefI ProjK x) -> Just x
      _ -> Nothing

isProjection :: C.Expr -> ScopeCheck (Maybe A.Name)
isProjection (C.Ident n) = isProjIdent n
isProjection _           = return Nothing
    
scopeCheckMeasure :: A.Measure C.Expr -> ScopeCheck (A.Measure A.Expr)
scopeCheckMeasure (A.Measure es) = do
  es' <- mapM scopeCheckExpr es
  return $ A.Measure es'
    
scopeCheckBound :: A.Bound C.Expr -> ScopeCheck (A.Bound A.Expr)
scopeCheckBound (A.Bound ltle e1 e2) = do
  [e1',e2'] <- mapM scopeCheckMeasure [e1,e2]
  return $ A.Bound ltle e1' e2'

checkPatternLength :: [C.Clause] -> Maybe Int
checkPatternLength [] = Just 0 -- arity 0
checkPatternLength (C.Clause _ pl _:cl) = cpl (length pl) cl
 where
   cpl k [] = Just k
   cpl k (C.Clause _ pl _ : cl) = if (length pl == k) then (cpl k cl) else Nothing

scopeCheckClause :: Maybe C.Name -> C.Clause -> ScopeCheck A.Clause
scopeCheckClause mname' (C.Clause mname pl mrhs) = do
  when (mname /= mname') $ errorClauseIdentifier mname mname'
  (pl, delta) <- runStateT (mapM scopeCheckPattern pl) emptyCtx
  local (addContext delta) $ do
    pl <- mapM scopeCheckDotPattern pl
    case mrhs of
      Nothing  -> return $ A.clause pl Nothing
      Just rhs -> A.clause pl . Just <$> scopeCheckExpr rhs

{-

scopeCheckClause :: Maybe C.Name -> C.Clause -> ScopeCheck A.Clause
scopeCheckClause mname' (C.Clause mname pl mrhs) = do
  when (mname /= mname') $ errorClauseIdentifier mname mname'
  pl' <- mapM scopeCheckPattern pl
  names <- collectVarPNames pl'
  pl'' <- local (addCtxs names) (zipWithM scopeCheckDotPattern pl pl') 
  case mrhs of
    Nothing -> return $ A.clause pl'' Nothing
    Just rhs -> do
      rhs' <- local (addCtxs names) (scopeCheckExpr rhs)
      return $ A.clause pl'' (Just rhs')

-- collect all variable names, checks if pattern is linear
collectVarPNames :: [A.Pattern] -> ScopeCheck [A.Name]
collectVarPNames pl = cvp pl [] where
    cvp [] acc = return acc
    cvp (p:pl) acc = case p of
                       A.ProjP{} -> cvp pl acc
                       A.AbsurdP -> cvp pl acc
                       A.SuccP p2 -> cvp (p2:pl) acc
                       A.ConP _ n pl2 -> cvp (pl2++pl) acc
                       A.PairP p1 p2 -> cvp (p1:p2:pl) acc
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
-}

type PatCtx = Context
type SPS = StateT PatCtx ScopeCheck

scopeCheckPattern :: C.Pattern -> SPS (A.Pat C.Name C.Expr)
scopeCheckPattern p = 
  case p of

    -- case n
    C.IdentP n -> do 
      sig <- lift $ getSig
      case lookupSig n sig of
        Just (DefI (ConK co) n) -> return $ A.ConP (A.PatternInfo co False) n [] 
                             -- a nullary constructor
        Just _  -> errorPatternNotConstructor n
        Nothing -> A.VarP <$> addUnique n
 
    -- case (i > j):  
    C.SizeP m n -> do
      -- m   <- lift $ scopeCheckLocalVar m
      A.SizeP m <$> addUnique n

    -- case $p
    C.SuccP p2    -> A.SuccP <$> scopeCheckPattern p2

    -- case (p1,p2)
    C.PairP p1 p2 -> A.PairP <$> scopeCheckPattern p1 <*> scopeCheckPattern p2

    -- case c ps
    C.ConP  n pl -> do 
      sig <- lift $ getSig
      case lookupSig n sig of
        Just (DefI (ConK co) n) -> 
          A.ConP (A.PatternInfo co False) n <$> mapM scopeCheckPattern pl  
        _  -> errorPatternNotConstructor n 

    -- case .e
    C.DotP e  -> do
      isProj <- lift $ isProjection e
      case isProj of 
       Just n  -> return $ A.ProjP n
       Nothing -> return $ A.DotP e -- dot patterns checked later

    -- case ()
    C.AbsurdP -> return $ A.AbsurdP

  where
    -- add to pattern context 
    addUnique :: C.Name -> SPS A.Name
    addUnique n = do
      delta <- get
      case lookupCtx n delta of
        Just{} -> errorPatternNotLinear n
        Nothing -> do
          let (x, delta') = insertCtx n delta
          put delta'
          return x
  
scopeCheckDotPattern :: A.Pat C.Name C.Expr -> ScopeCheck A.Pattern
scopeCheckDotPattern p = 
    case p of 
      A.DotP e -> A.DotP <$> scopeCheckExpr e
      A.SuccP p -> A.SuccP <$> scopeCheckDotPattern p
      A.ConP co n pl -> A.ConP co n <$> mapM scopeCheckDotPattern pl
      A.SizeP m n -> flip A.SizeP n <$> scopeCheckLocalVar m -- return $ A.SizeP m n
      A.VarP n    -> return $ A.VarP n
      A.ProjP n   -> return $ A.ProjP n
      A.AbsurdP   -> return $ A.AbsurdP
      -- impossible cases: ErasedP, UnusableP



{-          
type ForbiddenVars = [C.Name]



scopeCheckPattern :: 
     ForbiddenVars                -- ^ names already seen (for linearity check) 
  -> C.Pattern                    -- ^ the pattern to check
  -> (ForbiddenVars -> A.Pattern -> ScopeCheck a)  -- ^ the continuation
  -> ScopeCheck a
scopeCheckPattern forbidden p k =
  let contVar n f = if n `elem` forbidden then errorPatternNotLinear n
                    else addLocal n $ k (n:forbidden) . f

  case p of

    -- case n
    C.IdentP n -> do 
      sig <- getSig
      case lookupSig n sig of
        Just (ConK co) -> k forbidden $ A.ConP (A.PatternInfo co False) n [] 
                             -- a nullary constructor
        Just _  -> errorPatternNotConstructor n
        Nothing -> contVar n A.VarP
 
    -- case (i > j):  
    C.SizeP m n -> do
      m   <- scopeCheckLocalVar m
      contVar n $ A.SizeP m

    C.SuccP p2 -> scopeCheckPattern forbidden p2 $  
                     return $ A.SuccP p2'
    C.PairP p1 p2 -> do 
      p1' <- scopeCheckPattern p1
      p2' <- scopeCheckPattern p2
      return $ A.PairP p1' p2'
    C.ConP n pl -> do sig <- getSig
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
-}
{-
scopeCheckPattern :: C.Pattern -> ScopeCheck A.Pattern
scopeCheckPattern p = 
    case p of
      C.IdentP n -> do sig <- getSig
                       ctx <- asks context
                       case (lookupSig n sig) of
                         (Just (ConK co)) -> return $ A.ConP pi n [] -- a nullary constructor
                                             where pi = A.PatternInfo co False
                         (Just _) -> errorPatternNotConstructor n
                         Nothing -> return $ A.VarP n
      C.SizeP m n-> do -- cxt <- ask
                       -- when (not $ lookupCtx m cxt) $ -- check this later! (dotP)
                       --   errorIdentifierUndefined m
                       sig <- getSig
                       case (lookupSig n sig) of
                         (Just _) -> errorPatternNotConstructor n
                         Nothing -> return $ A.SizeP m n
      C.SuccP p2 -> do p2' <- scopeCheckPattern p2
                       return $ A.SuccP p2'
      C.PairP p1 p2 -> do 
        p1' <- scopeCheckPattern p1
        p2' <- scopeCheckPattern p2
        return $ A.PairP p1' p2'
      C.ConP n pl -> do sig <- getSig
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
-}

-- * Scope checking errors

errorAlreadyInSignature s n = throwErrorMsg $ show s  ++ ": Identifier " ++ n ++ " already in signature"

errorAlreadyInContext s n = throwErrorMsg $ show s ++ ": Identifier " ++ n ++ " already in context"

-- errorPatternNotVariable n = throwErrorMsg $ "pattern " ++ n ++ ": Identifier expected"

errorPatternNotConstructor n = throwErrorMsg $ "pattern " ++ n ++ " is not a constructor"

errorNotAField n = throwErrorMsg $ "record field " ++ n ++ " unknown"
-- errorUnknownProjection n = throwErrorMsg $ "projection " ++ n ++ " unknown"

errorDuplicateField r = throwErrorMsg $ show r ++ " assigns a field twice"


errorProjectionUsedAsExpression n = throwErrorMsg $ "projection " ++ n ++ " used as expression"

errorIdentifierUndefined n = throwErrorMsg $ "Identifier " ++ n ++ " undefined"

errorPatternNotLinear n = throwErrorMsg $ "pattern not linear: " ++ n 

errorClauseIdentifier (Just n) (Just n') = throwErrorMsg $ "Expected identifier " ++ show n' ++ " as clause head, found " ++ show n
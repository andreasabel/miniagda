-- NOTE: insertion of polarity variables disabled here, must be done
-- in TypeChecker

{-# LANGUAGE TupleSections, DeriveFunctor, GeneralizedNewtypeDeriving,
      FlexibleContexts, FlexibleInstances, UndecidableInstances,
      MultiParamTypeClasses #-}

module ScopeChecker (scopeCheck) where

import Polarity(Pol(..))
import qualified Polarity as A
import Abstract (Sized,mkExtRef,Co,ConK(..),PrePost(..),MVar,Decoration(..),Override(..),Measure(..),adjustTopDecsM,Arity)
import qualified Abstract as A
import qualified Concrete as C

import TraceError

import Prelude hiding (mapM)

import Control.Applicative -- <$>
import Control.Monad.IfElse
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Error hiding (mapM)

import Data.List as List
import Data.Maybe
import Data.Traversable (mapM)

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
  { stack             :: Stack     -- ^ Local names in scope.
    -- We keep a stack of these to disallow shadowing on the same level.
  , defaultPolarity   :: Pol       -- ^ Replacement for @Default@ polarity.
  , constraintAllowed :: Bool      -- ^ Is a constraint @|m| < |m'|@ legal now, since we just parsed a quantifier?
  }

type Stack = [Context]

initCtx :: SCCxt
initCtx = SCCxt
  { stack             = [[]]  -- one empty context to begin with
  , defaultPolarity   = A.Rec -- POL VARS DISABLED!!
  , constraintAllowed = False
  }

-- ** A lens for @constraintAllowed@

class LensConstraintAllowed a where
  mapConstraintAllowed :: (Bool -> Bool) -> a -> a
  setConstraintAllowed :: Bool -> a -> a
  setConstraintAllowed b = mapConstraintAllowed (const b)

instance LensConstraintAllowed SCCxt where
  mapConstraintAllowed f sc = sc { constraintAllowed = f (constraintAllowed sc) }

instance (LensConstraintAllowed r, MonadReader r m) => LensConstraintAllowed (m a) where
  mapConstraintAllowed f = local (mapConstraintAllowed f)

-- ** Managing the stack of local contexts.

newLevel :: ScopeCheck a -> ScopeCheck a
newLevel = local $ \ cxt -> cxt { stack = [] : stack cxt }

thisLevel :: SCCxt -> Context
thisLevel cxt = head (stack cxt)

instance Push Local SCCxt where
  push nx sc = sc { stack = push nx (stack sc) }

-- ** translating concrete names to abstract names

type Local   = (C.Name,A.Name)
type Context = [Local]

emptyCtx :: Context
emptyCtx = []

newLocal :: Push Local b => C.Name -> b -> (A.Name, b)
newLocal n cxt = (x, push (n, x) cxt)
  where x = A.fresh n

lookupLocal :: C.Name -> ScopeCheck (Maybe A.Name)
lookupLocal n = retrieve n <$> asks stack

lookupGlobal :: C.Name -> ScopeCheck (Maybe DefI)
lookupGlobal n = lookupSig n <$> getSig

addContext :: Context -> SCCxt -> SCCxt
addContext delta sc = sc { stack = delta : stack sc }

-- * Global identifiers.

-- | Kind of identifier.
data IKind
  = DataK
  | ConK ConK
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

-- | Add a local identifier.
--   (Not tail recursive, since it also returns the generate id.)
addBind' :: Show e => e -> C.Name -> (A.Name -> ScopeCheck a) -> ScopeCheck (A.Name, a)
addBind' e n k = do
  ctx <- ask
  case retrieve n (thisLevel ctx) of
    Just _  -> errorAlreadyInContext e n
    Nothing -> do
      let (x, ctx') = newLocal n ctx -- addCtx' n ctx
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
  let (x, ctx') = newLocal n ctx
  local (const ctx') $ k x

addTel :: C.Telescope -> A.Telescope -> ScopeCheck a -> ScopeCheck a
addTel ctel atel = local (addContext nxs)
  where nxs = reverse $ zipTels ctel atel

zipTels :: C.Telescope -> A.Telescope -> [(C.Name,A.Name)]
zipTels ctel atel = zip ns xs
  where ns = collectTelescopeNames ctel
        xs = map A.boundName atel

-- ** Global state.

getSig :: ScopeCheck Sig
getSig = ScopeCheck $ gets signature

-- | Add a global identifier.
addName :: IKind -> C.Name -> ScopeCheck A.Name
addName k n = do
  sig <- getSig
  when (isJust (lookupSig n sig)) $
    errorAlreadyInSignature "shadowing of global definitions forbidden" n
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

generalizeTBind :: C.TBind -> ScopeCheck C.TBind
generalizeTBind tb@C.TMeasure{} = return tb
generalizeTBind tb = do
  dec' <- generalizeDec (C.boundDec tb)
  return $ tb { C.boundDec = dec' }

-- | Insert polarity variables in telescope.
generalizeTel :: C.Telescope -> ScopeCheck C.Telescope
generalizeTel = mapM generalizeTBind

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

scopeCheckDeclaration (C.LetDecl eval letdef@C.LetDef{ C.letDefDec = dec, C.letDefName = n }) = do
  unless (dec == A.defaultDec) $
    throwErrorMsg $ "polarity annotation not supported in global let definition of " ++ show n
  (tel, mt, e) <- scopeCheckLetDef letdef
  x <- addName LetK n
  return $ A.LetDecl eval x tel mt e

scopeCheckDeclaration d@(C.PatternDecl n ns p) = do
  let errorHead = "invalid pattern declaration\n" ++ C.prettyDecl d ++ "\n"
  -- check pattern
  (p, delta) <- runStateT (scopeCheckPattern p) emptyCtx
  p <- local (addContext delta) $ scopeCheckDotPattern p
  -- ensure that pattern variables are the declared variables
  unless (sort ns == sort (map fst delta)) $ do
    let usedNames = map fst delta
        unusedNames = ns \\ usedNames
        undeclaredNames = usedNames \\ ns
    when (not (null unusedNames)) $ throwErrorMsg $
      errorHead ++ "unsed variables in pattern: "
        ++ Util.showList " " id unusedNames
    when (not (null undeclaredNames)) $ throwErrorMsg $
      errorHead ++ "undeclared variables in pattern: "
        ++ Util.showList " " id undeclaredNames
  --  when (n `elem` ns) $ throwErrorMsg $ errorHead ++ "pattern"
  x <- addName (ConK DefPat) n
  let xs = map (fromJust . flip lookup delta) ns
  return (A.PatternDecl x xs p)

-- we support
-- - mutual (co)funs
-- - mutual (co)data

scopeCheckDeclaration (C.MutualDecl []) = throwErrorMsg "empty mutual block"
scopeCheckDeclaration (C.MutualDecl l@(C.DataDecl{}:xl)) =
  scopeCheckMutual l
scopeCheckDeclaration (C.MutualDecl l@(C.FunDecl  co _ _:xl)) =
  scopeCheckFunDecls co l  -- >>= return . (:[])
scopeCheckDeclaration (C.MutualDecl _) = throwErrorMsg "mutual combination not supported"

scopeCheckLetDef :: C.LetDef -> ScopeCheck (A.Telescope, Maybe (A.Type), A.Expr)
scopeCheckLetDef (C.LetDef dec n tel mt e) =  setDefaultPolarity A.Rec $ do
  tel <- generalizeTel tel
  (tel, (mt, e)) <- scopeCheckTele tel $ do
     (,) <$> mapM scopeCheckExprN mt  -- allow shadowing after : in type
         <*> scopeCheckExprN e        -- allow shadowing after =
  return (tel, mt, e)

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

scopeCheckTele :: C.Telescope -> ScopeCheck a -> ScopeCheck (A.Telescope, a)
scopeCheckTele []         cont = ([],) <$> cont
scopeCheckTele (tb : tel) cont = do
  (tbs, (tel, a)) <- scopeCheckTBind tb $ scopeCheckTele tel cont
  return (tbs ++ tel, a)

scopeCheckTBind :: C.TBind -> ScopeCheck a -> ScopeCheck ([A.TBind], a)
scopeCheckTBind tb cont = do
  let contYes = setConstraintAllowed True  cont
      contNo  = setConstraintAllowed False cont
  case tb of
    C.TBind dec [] t -> do -- non-dependent function type
      t       <- scopeCheckExprN t
      ([A.noBind $ A.Domain t A.defaultKind dec],) <$> contNo
    C.TBind dec ns t -> do
      t       <- scopeCheckExprN t
      (xs, a) <- addBinds tb ns $ contYes
      return (map (\ x -> A.TBind x (A.Domain t A.defaultKind dec)) xs, a)
    C.TBounded dec n ltle e -> do
      e <- scopeCheckExprN e
      (x, a) <- addBind tb n $ contYes
      return ([A.TBind x (A.Domain (A.Below ltle e) A.defaultKind dec)], a)
    C.TMeasure mu -> do
      mu <- scopeCheckMeasure mu
      ([A.TMeasure mu],) <$> cont
--    C.TMeasure mu -> throwErrorMsg $ "measure not allowed in telescope"
    C.TBound beta -> do
      unlessM (asks constraintAllowed) $
        errorConstraintNotAllowed beta
      beta <- scopeCheckBound beta
      ([A.TBound beta],) <$> cont

checkBody :: (A.TypeSig, C.Declaration) -> ScopeCheck A.Declaration
checkBody (A.TypeSig x tt, C.DataDecl n sz co tel _ cs fields) =
  checkDataBody tt n x sz co tel cs fields
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
mutualGetTypeSig (C.LetDecl ev (C.LetDef dec n tel Nothing e)) =
  error $ "let declaration of " ++ show n ++ ": type required in mutual block"
mutualGetTypeSig (C.LetDecl ev (C.LetDef dec n tel (Just t) e)) =
  (LetK, C.TypeSig n (C.teleToType tel t))
{- mutualGetTypeSig (C.LetDecl ev tsig e) =
  (LetK, tsig) -}
mutualGetTypeSig (C.OverrideDecl _ [d]) =
  mutualGetTypeSig d


scopeCheckRecordDecl :: C.Name -> C.Telescope -> C.Type -> C.Constructor -> [C.Name] ->
  ScopeCheck A.Declaration
scopeCheckRecordDecl n tel t c cfields = enter n $ do
  setDefaultPolarity A.Param $ do
    tel <- generalizeTel tel
    -- STALE COMMENT: we do not infer at all: -- do not infer polarities in index arguments
    (A.TypeSig x tt') <- scopeCheckTypeSig (C.TypeSig n $ C.teleToType tel t)
    addAName DataK n x
    let names = collectTelescopeNames tel
        target = C.App (C.Ident n) (map C.Ident names)  -- R pars
        (tel',t') = A.typeToTele' (length names) tt'
    c' <- scopeCheckConstructor n (zipTels tel tel') A.CoInd target c
    let delta = contextFromConstructors c c'
    afields <- addFields ProjK delta cfields
    return $ A.RecordDecl x tel' t' c' afields

contextFromConstructors :: C.Constructor -> A.Constructor -> Context
contextFromConstructors (C.Constructor _ ctel0 mct) (A.Constructor _ _ at) = delta
  where ctel = maybe [] (fst . C.typeToTele) mct
        (atel, _) = A.typeToTele at
        delta = zipTels (ctel0 ++ ctel) atel

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
  setDefaultPolarity A.Param $ do
    tel <- generalizeTel tel0
    -- STALE: -- do not infer polarities in index arguments
    (A.TypeSig x tt') <- scopeCheckTypeSig (C.TypeSig n $ C.teleToType tel t)
    addAName DataK n x
    checkDataBody tt' n x sz co tel cs fields

-- precondition: name already added to signature
checkDataBody :: A.Type -> C.Name -> A.Name -> Sized -> Co -> C.Telescope -> [C.Constructor] -> [C.Name] -> ScopeCheck A.Declaration
checkDataBody tt' n x sz co tel cs fields = do
      let cnames = collectTelescopeNames tel         -- parameters
          target = C.App (C.Ident n) $ map C.Ident cnames  -- D pars
          (tel',t') = A.typeToTele' (length cnames) tt'
      cs' <- mapM (scopeCheckConstructor n (zipTels tel tel') co target) cs
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
      -- fields <- addFields (LetK) delta fields
      -- 2012-01-26 register as projections
      fields <- addFields ProjK delta fields
      let pos = map (A.polarity . A.decor . A.boundDom) tel'
      return $ A.DataDecl x sz co pos tel' t' cs' fields

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
      C.Quant A.Pi [C.TMeasure mu] e1 -> do
        mu' <- scopeCheckMeasure mu
        e1' <- scopeCheckExprN e1
        return (Just $ length (measure mu'), A.pi (A.TMeasure mu') e1')

      -- bounds are allowed here, since we check a function type
      C.Quant A.Pi [C.TBound beta] e1 -> do
        beta'     <- scopeCheckBound beta
        (ml, e1') <- scopeCheckFunType e1
        return (ml, A.pi (A.TBound beta') e1')

      C.Quant A.Pi tel e -> do
        tel <- generalizeTel tel
        (tel, (ml, e)) <- setDefaultPolarity A.Rec $ setConstraintAllowed False $
          scopeCheckTele tel $ setConstraintAllowed True $ scopeCheckFunType e
        ml' <- findMeasure tel
        ml <- case (ml,ml') of
                 (Nothing,ml') -> return ml'
                 (ml, Nothing) -> return ml
                 (Just{}, Just{}) -> errorOnlyOneMeasure
        return (ml, A.teleToType tel e)

      t -> (Nothing,) <$> scopeCheckExpr t -- no measure found

findMeasure :: A.Telescope -> ScopeCheck (Maybe Int)
findMeasure tel =
  case [ mu | A.TMeasure mu <- tel ] of
    []           -> return Nothing
    [Measure mu] -> return $ Just $ length mu
    _            -> errorOnlyOneMeasure

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

-- | @cxt@ is the data telescope.
scopeCheckConstructor :: C.Name -> Context -> Co -> C.Type -> C.Constructor -> ScopeCheck A.Constructor
scopeCheckConstructor d cxt co t0 a@(C.Constructor n tel mt) =
  checkInSig a n $ \ x -> do

  let finish t mcxt = local (addContext $ maybe cxt id mcxt) $ do
       t <- setDefaultPolarity A.Param $ scopeCheckExpr $ C.teleToType tel t
       t <- adjustTopDecsM defaultToParam t
       addAName (ConK $ A.coToConK co) n x
       let dummyDom = A.Domain A.Irr A.NoKind $ A.Dec Param
           mtel     = fmap (map (\ (n,x) -> A.TBind x dummyDom)) mcxt
           ps       = [] -- patterns computed during type checking
       return $ A.Constructor x (fmap (,ps) mtel) t

  case mt of

    -- no target given, then add the data tel to the scope
    Nothing -> finish t0 Nothing

    -- target given, then the target binds the parameter names
    Just t -> do
      -- get the final target
      let (_, target) = C.typeToTele t

          fallback = finish t Nothing
          continue d' es = do
            -- unless (d == d') $ errorWrongTarget n d d'
            if (d /= d') then fallback else do
            -- get the parameters of target
            let (pars, inds) = splitAt (length cxt) es
            unless (length pars == length cxt) $ errorNotEnoughParameters n target
            -- if parameters are just data parameters, do it old style
            if and (zipWith isTelPar cxt pars) then fallback else do
            -- scopeCheck the parameters as patterns
            finish t . Just =<< parameterVariables pars

      case target of
        C.Ident d'            -> continue d' []
        C.App (C.Ident d') es -> continue d' es
        _ -> fallback -- errorTargetMustBeAppliedName n target

{- OLD CODE
scopeCheckConstructor :: C.Telescope -> A.Telescope -> Co -> C.Type -> C.Constructor -> ScopeCheck A.Constructor
scopeCheckConstructor ctel atel co t0 a@(C.Constructor n tel mt) = addTel ctel atel $ checkInSig a n $ \ x -> do
    let t = maybe t0 id mt
    t <- setDefaultPolarity A.Param $ scopeCheckExpr $ C.teleToType tel t
    t <- adjustTopDecsM defaultToParam t
    addAName (ConK $ A.coToConK co) n x
    return $ A.TypeSig x t
-}
  where isTelPar (c,_) (C.Ident x) = c == x
        isTelPar _     _           = False
        defaultToParam dec = case (A.polarity dec) of
          A.Default -> return $ dec { A.polarity = A.Param }
          A.Param   -> return dec
          A.Const   -> return dec
          A.PVar{}  -> return dec
          _         -> fail $ "illegal polarity " ++ show (polarity dec) ++ " in type of constructor " ++ show a

-- | Allow shadowing of previous locals.
--   Always if we enter a subexpression which is not the body
--   of a binder.
scopeCheckExprN :: C.Expr -> ScopeCheck A.Expr
scopeCheckExprN = newLevel . scopeCheckExpr

scopeCheckExpr :: C.Expr -> ScopeCheck A.Expr
scopeCheckExpr e = setConstraintAllowed False $ scopeCheckExpr' e

scopeCheckExpr' :: C.Expr -> ScopeCheck A.Expr
scopeCheckExpr' e =
    case e of
      -- replace underscore by next meta-variable
      C.Unknown -> nextMVar (return . A.Meta)
      C.Set   e -> A.Sort . A.Set   <$> scopeCheckExprN e
      C.CoSet e -> A.Sort . A.CoSet <$> scopeCheckExprN e
      C.Size    -> return $ A.Sort (A.SortC A.Size)
      C.Succ e1 -> A.Succ <$> scopeCheckExprN e1
      C.Zero    -> return A.Zero
      C.Infty   -> return A.Infty
      C.Plus e1 e2 -> do
        e1 <- scopeCheckExprN e1
        e2 <- scopeCheckExprN e2
        return $ A.Plus [e1, e2]
      C.Pair e1 e2   -> A.Pair <$> scopeCheckExprN e1 <*> scopeCheckExprN e2
      C.Sing e1 et   -> A.Sing <$> scopeCheckExprN e1 <*> scopeCheckExprN et
      C.App C.Max el -> do
        el' <- mapM scopeCheckExprN el
        when (length el' < 2) $ throwErrorMsg "max expects at least 2 arguments"
        return $ A.Max el'
      C.App e1 el -> foldl A.App <$> scopeCheckExprN e1 <*> mapM scopeCheckExprN el
      C.Case e mt cl -> do
        e'  <- scopeCheckExprN e
        mt' <- mapM scopeCheckExprN mt
        cl' <- mapM (scopeCheckClause Nothing) cl
        return $ A.Case e' mt' cl'

      -- measure & bound
      -- measures can only appear in fun sigs!
      C.Quant pisig [C.TMeasure mu] e1 -> do
        fail $ "measure not allowed in expression " ++ show e

      -- measure bound mu < mu'
      C.Quant A.Pi [C.TBound beta] e1 -> do
        unlessM (asks constraintAllowed) $ errorConstraintNotAllowed beta
        beta' <- scopeCheckBound beta
        e1'   <- scopeCheckExpr' e1
        return $ A.pi (A.TBound beta') e1'

      C.Quant A.Sigma [C.TBound beta] e1 -> fail $
        "measure bound not allowed in expression " ++ show e

      C.Quant pisig tel e -> do
        tel <- generalizeTel tel
        pol <- asks defaultPolarity
        (tel, e) <- setDefaultPolarity A.Rec $ setConstraintAllowed False $ scopeCheckTele tel $
           setDefaultPolarity pol $ scopeCheckExpr' e
        return $ quant pisig tel e where
--          quant A.Sigma [tb] = A.Quant A.Sigma tb
          quant A.Sigma tel e = foldr (A.Quant A.Sigma) e tel
          quant A.Pi    tel e = A.teleToType tel e

      C.Lam n e1 -> do
        (n, e1') <- addBind e n $ scopeCheckExpr e1
        return $ A.Lam A.defaultDec n e1' -- dec. in Lam is ignored in t.c.

      C.LLet letdef e2 -> do
        let dec = C.letDefDec letdef
        (tel, mt, e1) <- scopeCheckLetDef letdef
        (x, e2) <- addBind e (C.letDefName letdef) $ scopeCheckExpr e2
        return $ A.LLet (A.TBind x $ A.Domain mt A.defaultKind dec) tel e1 e2

      C.Record rs -> do
        let fields = map fst rs
        if (hasDuplicate fields) then (errorDuplicateField e) else do
          rs <- mapM scopeCheckRecordLine rs
          return $ A.Record A.AnonRec rs

      C.Proj n -> A.Proj Post <$> scopeCheckProj n

      C.Ident n -> do
        res <- lookupLocal n
        case res of
          Just n -> return $ A.Var n
          Nothing -> do
            res <- lookupGlobal n
            case res of
             Just (DefI k x) -> case k of
               (ConK co)  -> return $ A.con co x
               LetK       -> return $ A.letdef x
               -- references to recursive functions are coded differently
               -- outside the mutual block
               FunK True  -> return $ A.fun x -- A.letdef x -- A.mkExtRef x
               FunK False -> return $ A.fun x
               DataK      -> return $ A.dat x
               ProjK      -> return $ A.Proj A.Pre x -- errorProjectionUsedAsExpression n
             Nothing -> errorIdentifierUndefined n
      _ -> fail $ "NYI: scopeCheckExpr " ++ show e

scopeCheckLocalVar :: C.Name -> ScopeCheck A.Name
scopeCheckLocalVar n = maybe (errorIdentifierUndefined n) return =<< do
  lookupLocal n

scopeCheckRecordLine :: ([C.Name], C.Expr) -> ScopeCheck (A.Name, A.Expr)
scopeCheckRecordLine (n : ns, e) = do
  x <- scopeCheckProj n
  (x,) <$> scopeCheckExprN (foldr C.Lam e ns)

scopeCheckProj :: C.Name -> ScopeCheck A.Name
scopeCheckProj n = do
  sig <- getSig
  case lookupSig n sig of
    Just (DefI ProjK x) -> return x
    _                   -> errorNotAField n


-- | @isProjIdent n = n@ if defined and the name of a projection.
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
  es' <- mapM scopeCheckExprN es
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
      Just rhs -> A.clause pl . Just <$> scopeCheckExprN rhs


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

-- | Add pattern variable to pattern context, must not be present yet.
addUnique :: C.Name -> SPS A.Name
addUnique n = do
  delta <- get
  case retrieve n delta of
    Just{} -> errorPatternNotLinear n
    Nothing -> do
      let (x, delta') = newLocal n delta
      put delta'
      return x

scopeCheckDotPattern :: A.Pat C.Name C.Expr -> ScopeCheck A.Pattern
scopeCheckDotPattern p =
    case p of
      A.DotP e -> A.DotP <$> scopeCheckExprN e
      A.PairP p1 p2 -> A.PairP <$> scopeCheckDotPattern p1 <*>  scopeCheckDotPattern p2
      A.SuccP p -> A.SuccP <$> scopeCheckDotPattern p
      A.ConP co n pl -> A.ConP co n <$> mapM scopeCheckDotPattern pl
--      A.SizeP m n -> flip A.SizeP n <$> scopeCheckLocalVar m -- return $ A.SizeP m n
      A.SizeP e n    -> flip A.SizeP n <$> scopeCheckExprN e
      A.VarP n       -> return $ A.VarP n  -- even though p = A.VarP n, it has wrong type!!
      A.ProjP n      -> return $ A.ProjP n
      A.AbsurdP      -> return $ A.AbsurdP
      -- impossible cases: ErasedP, UnusableP


-- * Scope checking parameters

parameterVariables :: [C.Expr] -> ScopeCheck Context
parameterVariables es = do
  execStateT (mapM_ scopeCheckParameter es) emptyCtx

-- | Extract variables bound by data parameters.
--   We consider a more liberal set of patterns, everything
--   that is injective and does not bind variables.
scopeCheckParameter :: C.Expr -> SPS ()
scopeCheckParameter e =
  case e of
    C.Set e'             -> scopeCheckParameter e'
    C.CoSet e'           -> scopeCheckParameter e'
    C.Size               -> return ()
    C.Succ e'            -> scopeCheckParameter e'
    C.Zero               -> return ()
    C.Infty              -> return ()
    C.Pair e1 e2         -> scopeCheckParameter e1 >> scopeCheckParameter e2
    C.Record fs          -> mapM_ (scpField e) fs
    C.Ident n            -> scpApp e n []
    C.App (C.Ident n) es -> scpApp e n es
    C.App C.App{} es     -> fail $ "scopeCheckParameter " ++ show e ++ ": internal invariant violated"
    _ -> errorInvalidParameter e
  where
    -- we can only treat a record expression as pattern
    -- if it does not bind any variables
    scpField :: C.Expr -> ([C.Name], C.Expr) -> SPS ()
    scpField e ([f], e') = scopeCheckParameter e'
    scpField e _         = errorInvalidParameter e

    scpApp :: C.Expr -> C.Name -> [C.Expr] -> SPS ()
    scpApp e n es = do
      sig <- lift $ getSig
      case lookupSig n sig of
        Just (DefI ConK{} n) -> mapM_ scopeCheckParameter es
        Just (DefI DataK  n) -> mapM_ scopeCheckParameter es
        Just _  -> errorInvalidParameter e
        Nothing -> void $ addUnique n

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

errorOnlyOneMeasure = throwErrorMsg "only one measure allowed in a function type"

errorConstraintNotAllowed beta = throwErrorMsg $
  show beta ++ ": constraints must follow a quantifier"

errorTargetMustBeAppliedName n t = throwErrorMsg $
  "constructor " ++ n ++ ": target must be data/record type applied to parameters and indices; however, I found " ++ show t

errorWrongTarget c d d' = throwErrorMsg $
  "constructor " ++ c ++ " should target data/record type " ++ d ++ "; however, I found " ++ d'

errorNotEnoughParameters c t = throwErrorMsg $
  "constructor " ++ c ++ ": target " ++ show t ++ " is missing parameters"

errorInvalidParameter e = throwErrorMsg $
  "expression " ++ show e ++ " is not valid in a parameter"

{-# LANGUAGE TypeSynonymInstances, PatternGuards #-}
 
module TCM where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

import Control.Applicative
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable) 
import qualified Data.Traversable as Traversable
import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Debug.Trace

import Abstract
import Polarity
import Value
import {-# SOURCE #-} Eval -- (up,whnf')
import PrettyTCM

-- import CallStack 
import TraceError

import TreeShapedOrder (TSO)
import qualified TreeShapedOrder as TSO

import Util

import Warshall


traceRew msg a = a -- trace msg a 
traceRewM msg = return () -- traceM msg
{-
traceRew msg a = trace msg a 
traceRewM msg = traceM msg
-}

-- metavariables and constraints

{-
traceMeta msg a = a -- trace msg a 
traceMetaM msg = return () -- traceM msg
-}
traceMeta msg a = trace msg a 
traceMetaM msg = traceM msg


-- lists of exactly one or two elements ------------------------------

-- this would have been better implemented by just lists and a view 
--   type OneOrTwo a = [a]
--   data View12 a = One a | Two a a  
--   fromList12 
-- then one could still get completeness of pattern matching!
-- now we have lots of boilerplate code

data OneOrTwo a = One a | Two a a deriving (Eq, Ord)

instance Show a => Show (OneOrTwo a) where
  show (One a)   = show a
  show (Two a b) = show a ++ "||" ++ show b

name12 :: OneOrTwo Name -> Name
name12 (One n) = n
name12 (Two n1 n2) = n1 ++ "||" ++ n2

instance Functor OneOrTwo where
  fmap f (One a)   = One (f a)
  fmap f (Two a b) = Two (f a) (f b)

instance Foldable OneOrTwo where
  foldMap f (One a) = f a
  foldMap f (Two a b) = f a `mappend` f b

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)	
instance Traversable OneOrTwo where
  traverse f (One a) = One <$> f a
  traverse f (Two a b) = Two <$> f a <*> f b

-- eliminator
oneOrTwo :: (a -> b) -> (a -> a -> b) -> OneOrTwo a -> b
oneOrTwo f g (One a) = f a
oneOrTwo f g (Two a1 a2) = g a1 a2

fromOne :: OneOrTwo a -> a
fromOne (One a) = a

toTwo :: OneOrTwo a -> OneOrTwo a
toTwo = oneOrTwo (\ a -> Two a a) Two

first12 :: OneOrTwo a -> a
first12 (One a) = a
first12 (Two a1 a2) = a1

second12 :: OneOrTwo a -> a
second12 (One a) = a
second12 (Two a1 a2) = a2

mapSecond12 :: (a -> a) -> OneOrTwo a -> OneOrTwo a
mapSecond12 f (One a) = One (f a)
mapSecond12 f (Two a1 a2) = Two a1 (f a2)

zipWith12 :: (a -> b -> c) -> OneOrTwo a -> OneOrTwo b -> OneOrTwo c
zipWith12 f (One a) (One b) = One (f a b)
zipWith12 f (Two a a') (Two b b') = Two (f a b) (f a' b')

zipWith123 :: (a -> b -> c -> d) -> 
              OneOrTwo a -> OneOrTwo b -> OneOrTwo c -> OneOrTwo d
zipWith123 f (One a) (One b) (One c) = One (f a b c)
zipWith123 f (Two a a') (Two b b') (Two c c') = Two (f a b c) (f a' b' c')

toList12 :: OneOrTwo a -> [a]
toList12 (One a) = [a]
toList12 (Two a1 a2) = [a1,a2]

fromList12 :: Show a => [a] -> OneOrTwo a 
fromList12 [a]     = One a
fromList12 [a1,a2] = Two a1 a2
fromList12 l = error $ "fromList12 " ++ show l

toMaybe12 :: Show a => [a] -> Maybe (OneOrTwo a) 
toMaybe12 []      = Nothing
toMaybe12 [a]     = Just $ One a
toMaybe12 [a1,a2] = Just $ Two a1 a2
toMaybe12 l = error $ "toMaybe12 " ++ show l


-- reader monad for local environment 
-- COMMENT "(not used right now)" <-- what does this mean?

data TCContext = TCContext 
  { context   :: SemCxt
  , renaming  :: Ren       -- assigning de Bruijn Levels to names
  , naming    :: Map Int Name  -- assigning names to de Bruijn levels
--  , nameVariants :: Map Name Int -- how many variants of the name
  , environ   :: Env2
  , rewrites  :: Rewrites
  , sizeRels  :: TSO Int   -- relations of universal (rigid) size variables
                           -- collected from size patterns (x > y)
  , bounds    :: [Bound Val]  -- bound hyps that do not fit in sizeRels
  , checkingConType :: Bool  -- different PTS rules for constructor types (parametric function space!)
  , assertionHandling :: AssertionHandling -- recover from errors?
  , impredicative :: Bool       -- use impredicative PTS rules
  -- checking measured functions 
  , funsTemplate :: Map Name (Kinded Fun) -- types of mutual funs with measures checking body
  , mutualFuns :: Map Name SigDef -- types of mutual funs while checking body
  , mutualCo :: Co                -- mutual block (co)recursive ?
  , checkingMutualName :: Maybe DefId -- which body of a mutual block am I checking?
  } 

instance Show TCContext where
    show ce = show (environ ce) ++ "; " ++ show (context ce)

emptyContext = TCContext 
  { context  = cxtEmpty
  , renaming = Map.empty 
  , naming   = Map.empty
  , environ  = emptyEnv 
  , rewrites = emptyRewrites
  , sizeRels = TSO.empty
  , bounds   = []
  , checkingConType = False
  , assertionHandling = Failure  -- default is not to ignore any errors
  , impredicative = False
  , funsTemplate = Map.empty
  , mutualFuns = Map.empty
  , mutualCo = Ind
  , checkingMutualName = Nothing
  }

-- state monad for global signature

data TCState = TCState 
  { signature   :: Signature
  , metaVars    :: MetaVars
  , constraints :: Constraints
  , positivityGraph :: PositivityGraph
  }

type MetaVars = Map MVar MetaVar
emptyMetaVars = Map.empty

type MScope = [Name] -- ^ names of size variables which are in scope of mvar
data MetaVar = MetaVar 
  { mscope   :: MScope
  , solution :: Maybe Val
  }

type PosConstrnt = Constrnt PPoly DefId ()
type PositivityGraph = [PosConstrnt]
emptyPosGraph = []

-- type TypeCheck = StateT TCState (ReaderT TCContext (CallStackT String IO)) 
type TypeCheck = StateT TCState (ReaderT TCContext (ErrorT TraceError IO)) 

instance MonadAssert TypeCheck where
  assert b s = do
    h <- asks assertionHandling
    assert' h b s
  newAssertionHandling h = local ( \ ce -> ce { assertionHandling = h })

{- mtl-2 provides these instances
-- TypeCheck is applicative since every monad is.  
-- I do not know why this ain't in the libraries...
instance Applicative TypeCheck where
  pure      = return
  mf <*> ma = mf >>= \ f -> ma >>= \ a -> pure (f a)
-}
 
-- rewriting rules -----------------------------------------------

data Rewrite  = Rewrite { lhs :: Val,  rhs :: Val }
type Rewrites = [Rewrite]

emptyRewrites = []

instance Show Rewrite where
  show rr = show (lhs rr) ++ " --> " ++ show (rhs rr)

{- renaming ------------------------------------------------------

  A renaming maps names to de Bruijn levels (= generic values).
-}

type Ren = Map Name Int

type Env2 = Environ (OneOrTwo Val)

type Context a = Map Int a
type Context2 a = Context (OneOrTwo a)

{- context -------------------------------------------------------

A context maps generic values to their type value.

During type checking, named variables are mapped to
generic values via a renaming.  Thus, looking up the type of a
name involves first looking up the generic value, and then its type.

-}

{-
-- data Domain = Domain { typ :: TVal, decor :: Dec } 
data Domain = Domain { typ :: TVal, kind :: Class, decor :: Dec } 

mapTyp :: (TVal -> TVal) -> Domain -> Domain
mapTyp f dom = dom { typ = f (typ dom) }

mapTypM :: Monad m => (TVal -> m TVal) -> Domain -> m Domain
mapTypM f dom = do
  t' <- f (typ dom) 
  return $ dom { typ = t' }

instance Show Domain where
  show item = (if erased (decor item) then brackets else id) (show (typ item))
-}

-- During heterogeneous equality, a variable might have
-- two different types, one on the left and one on the right.
-- We implement this as Two tl tr.

data CxtE a = CxtEntry { domain :: a, upperDec :: UDec }
type CxtEntry  = CxtE (OneOrTwo Domain)
type CxtEntry1 = CxtE Domain

data SemCxt = SemCxt 
  { len   :: Int
  , cxt   :: Context2 Domain  -- fixed part of context
  , upperDecs :: Context UDec -- the "should be below" decoration for each var.; this is updated by resurrection 
  }
{- invariant: length (cxt delta) = length (upperDecs delta) = len 
     cxt(i) = Two ... iff  upperDecs(i) = Two ...
 -}

instance Show SemCxt where
  show delta = 
    show $ zip (Map.elems (cxt delta)) 
               (Map.elems (upperDecs delta))
{-
  show delta = show $ zip ( 
    zipWith3 (zipWith12 Domain) 
--    zipWith (\ entry dec -> fmap ((flip Domain) dec) entry) 
      (Map.elems (cxt delta))
      (Map.elems (kinds delta))
      (Map.elems (decs delta))
    ) (Map.elems (upperDecs delta))
-}
cxtEmpty = SemCxt 
  { len = 0
  , cxt = Map.empty
--  , kinds = Map.empty
--  , decs = Map.empty 
  , upperDecs = Map.empty
  }

-- push a new type declaration on context
cxtPush' :: OneOrTwo Domain -> SemCxt -> SemCxt
cxtPush' entry delta = 
  delta { len = k + 1
        , cxt  = Map.insert k entry (cxt delta)
--        , cxt  = Map.insert k (fmap typ   entry) (cxt delta)
--        , decs = Map.insert k (fmap decor entry) (decs delta) 
        , upperDecs = Map.insert k defaultUpperDec (upperDecs delta)
        }
  where k = len delta
{-
cxtPush' (tv12, dec) delta = 
  delta { len = k + 1
        , cxt  = Map.insert k tv12 (cxt delta)
        , decs = Map.insert k dec (decs delta) }
  where k = len delta
-}
{-
cxtPush :: Dec -> TVal -> SemCxt -> (Int, SemCxt)
cxtPush dec v delta = (len delta, cxtPush' (One (Domain v dec)) delta)
-- cxtPush dec v delta = (len delta, cxtPush' (One v, dec) delta)
-}

cxtPushEntry :: OneOrTwo Domain -> SemCxt -> (Int, SemCxt)
cxtPushEntry ce delta = (len delta, cxtPush' ce delta)

cxtPush :: Domain -> SemCxt -> (Int, SemCxt)
cxtPush dom delta = cxtPushEntry (One dom) delta
-- cxtPush dec v delta = (len delta, cxtPush' (One v, dec) delta)

-- push a variable with a left and a right type
cxtPush2 :: Domain -> Domain -> SemCxt -> (Int, SemCxt)
cxtPush2 doml domr delta = cxtPushEntry (Two doml domr) delta
--  (len delta, cxtPush' (Two doml domr) delta)

{-
-- push a variable with a left and a right type
cxtPush2 :: Dec -> TVal -> TVal -> SemCxt -> (Int, SemCxt)
cxtPush2 dec tvl tvr delta = 
  (len delta, cxtPush' (Two tvl tvr, dec) delta)
-}

cxtPushGen ::  Name -> SemCxt -> (Int, SemCxt)
cxtPushGen x delta = cxtPush bot delta
  where bot = error $ "IMPOSSIBLE: name " ++ x ++ " is not bound to any type"

-- only defined for single bindings
cxtSetType :: Int -> Domain -> SemCxt -> SemCxt
cxtSetType k dom delta = 
  delta { cxt  = Map.insert k (One dom) (cxt delta)
        -- upperDecs need not be updated
        }

{-
-- only defined for single bindings
cxtSetType :: Int -> Dec -> TVal -> SemCxt -> SemCxt
cxtSetType k dec tv delta = 
  delta { cxt  = Map.insert k (One tv) (cxt delta)
        , decs = Map.insert k (One dec) (decs delta) 
        -- upperDecs need not be updated
        }
--        , decs = Map.insert k dec (decs delta) }
-}
{-
cxtLookupGen :: Monad m => SemCxt -> Int -> m Domain
cxtLookupGen delta k = do
  One tv  <- lookupM k (cxt delta)
  One dec <- lookupM k (decs delta)
--  dec    <- lookupM k (decs delta)
  return $ Domain { typ = tv, decor = dec }

cxtLookupGen :: Monad m => SemCxt -> Int -> m CxtEntry
cxtLookupGen delta k = do
  tv12  <- lookupM k (cxt delta)
  dec12 <- lookupM k (decs delta)
  udec  <- lookupM k (upperDecs delta)
  return $ CxtEntry (zipWith12 Domain tv12 dec12) udec
-}
cxtLookupGen :: Monad m => SemCxt -> Int -> m CxtEntry
cxtLookupGen delta k = do
  dom12 <- lookupM k (cxt delta)
  udec  <- lookupM k (upperDecs delta)
  return $ CxtEntry dom12 udec

cxtLookupName :: Monad m => SemCxt -> Ren -> Name -> m CxtEntry
cxtLookupName delta ren x = do
  i <- lookupM x ren
  cxtLookupGen delta i

{-
cxtLookupName :: Monad m => SemCxt -> Ren -> Name -> m Domain
cxtLookupName delta ren x = do
  i <- lookupM x ren
  cxtLookupGen delta i
-}

-- apply decoration, possibly resurrecting (see Pfenning, LICS 2001)
-- and changing polarities (see Abel, MSCS 2008)
cxtApplyDec :: Dec -> SemCxt -> SemCxt
cxtApplyDec dec delta = delta { upperDecs = Map.map (compDec dec) (upperDecs delta) }
-- cxtApplyDec dec delta =  delta { decs = Map.map (fmap $ invCompDec dec) (decs delta) }

{- RETIRED, use cxtApplyDec instead
-- clear all "erased" flags (see Pfenning, LICS 2001)
-- UPDATE: resurrection sets "target" status to erased 
--         (as opposed to setting "source" status to non-erased)
cxtResurrect :: SemCxt -> SemCxt
cxtResurrect delta = delta { upperDecs = Map.map (\ dec -> dec { erased = True}) (upperDecs delta) }
-- cxtResurrect delta = delta { decs = Map.map (fmap resurrectDec) (decs delta) }
-}

-- manipulating the context ------------------------------------------

class Monad m => MonadCxt m where
--  bind     :: Name -> Domain -> Val -> m a -> m a
--  new performs eta-expansion "up" of new gen 
  -- adding types (Two t1 t2) returns values (Two (Up t1 vi) (Up t2 vi))
  newVar     :: Name -> OneOrTwo Domain -> (Int -> OneOrTwo Val -> m a) -> m a 
  newWithGen :: Name -> Domain -> (Int -> Val -> m a) -> m a 
  newWithGen x d k = newVar x (One d)
    (\ i (One v) -> k i v)
  new2WithGen:: Name -> (Domain, Domain) -> (Int -> (Val, Val) -> m a) -> m a   
  new2WithGen x (doml, domr) k = newVar x (Two doml domr) 
    (\ i (Two vl vr) -> k i (vl, vr))
{-
  new2WithGen:: Name -> (TVal, TVal, Dec) -> (Int -> (Val, Val) -> m a) -> m a   
  new2WithGen x (tvl, tvr, dec) k = newVar x (Two tvl tvr, dec) 
    (\ i (Two vl vr) -> k i (vl, vr))
-}
  new        :: Name -> Domain -> (Val -> m a) -> m a
  new x d cont = newWithGen x d (\ _ -> cont)   
  new2       :: Name -> (Domain, Domain) -> ((Val, Val) -> m a) -> m a
  new2 x d cont = new2WithGen x d (\ _ -> cont)   
{-
  new2       :: Name -> (TVal, TVal, Dec) -> ((Val, Val) -> m a) -> m a
  new2 x d cont = new2WithGen x d (\ _ -> cont)   
-}
  new'       :: Name -> Domain -> m a -> m a
  new' x d cont = new x d (\ _ -> cont) 
  newIrr     :: Name -> m a -> m a  -- only add binding x = VIrr to env
  addName    :: Name -> (Val -> m a) -> m a  
{- RETIRED
  addTypeSigs :: [TySig TVal] -> m a -> m a
  addTypeSigs [] k = k
  addTypeSigs (TypeSig n tv : tss) k = 
    new' n (defaultDomain tv) $ addTypeSigs tss k
-}
  addKindedTypeSigs :: [Kinded (TySig TVal)] -> m a -> m a
  addKindedTypeSigs [] k = k
  addKindedTypeSigs (Kinded ki (TypeSig n tv) : ktss) k = 
    new' n (Domain tv ki defaultDec) $ addKindedTypeSigs ktss k
--  addName x = new x dontCare
  setType    :: Int -> Domain -> m a -> m a
  setTypeOfName :: Name -> Domain -> m a -> m a
  genOfName  :: Name -> m Int
  nameOfGen  :: Int -> m Name
  nameTaken  :: Name -> m Bool
  uniqueName :: Name -> Int -> m Name
  uniqueName x k = ifM (nameTaken x) (return $ x ++ "~" ++ show k) (return x)
  lookupGen  :: Int -> m CxtEntry
  lookupGenType2 :: Int -> m (TVal, TVal)
  lookupGenType2 i = do
    entry <- lookupGen i 
    case domain entry of
      One d1    -> return (typ d1, typ d1)
      Two d1 d2 -> return (typ d1, typ d2)
  lookupName :: Name -> m CxtEntry
  lookupName1 :: Name -> m CxtEntry1
  lookupName1 x = do
    e <- lookupName x
    return $ CxtEntry (fromOne (domain e)) (upperDec e)

  getContextTele :: m TeleVal  -- return context as telescope of type values
  getLen     :: m Int       -- return length of the context
  getEnv     :: m Env       -- return current environment
  getRen     :: m Ren       -- return current renaming
  applyDec   :: Dec -> m a -> m a  -- resurrect/adjust polarities
  resurrect  :: m a -> m a -- resurrect all erased variables in context
  resurrect = applyDec irrelevantDec
  addRewrite :: Rewrite -> [Val] -> ([Val] -> m a) -> m a
  addPattern :: TVal -> Pattern -> Env -> (TVal -> Val -> Env -> m a) -> m a -- step under pat
  addPatterns:: TVal -> [Pattern] -> Env -> (TVal -> [Val] -> Env -> m a) -> m a
  addSizeRel  :: Int -> Int -> Int -> m a -> m a  
  addBoundHyp :: Bound Val -> m a -> m a
  sizeVarBelow :: Int -> Int -> m (Maybe Int)
--  getSizeDiff :: Int -> Int -> m (Maybe Int)
  getMinSize  :: Int -> m (Maybe Int)
  getSizeVarsInScope :: m [Name]
  checkingCon :: Bool -> m a -> m a
  checkingDom :: m a -> m a  -- check domain A of Pi x:A.B (takes care of polarities)
  setCo :: Co -> m a -> m a -- entering a recursive or corecursive function?
  installFuns :: Co -> [Kinded Fun] -> m a -> m a
  setMeasure  :: Measure Val -> m a -> m a 
  activateFuns :: m a -> m a -- create instance of mutually recursive functions bounded by measure
  goImpredicative :: m a -> m a
  checkingMutual :: Maybe DefId -> m a -> m a

dontCare = error "Internal error: tried to retrieve unassigned type of variable" 

instance MonadCxt TypeCheck where

  newIrr x = local (\ ce -> ce { environ = update (environ ce) x (One VIrr) })

  -- UPDATE to 2?
  addName x f = enter ("new " ++ x ++ " : _") $ do
    cxtenv <- ask
    let (k, delta) = cxtPushGen x (context cxtenv)
    let v = VGen k
    let rho = update (environ cxtenv) x (One v) 
    x' <- uniqueName x k
    local (\ cxt -> cxt { context = delta
                        , renaming = Map.insert x k (renaming cxtenv)
                        , naming = Map.insert k x' (naming cxt)
                        , environ = rho }) (f v)

  newVar x dom12 f = do 
    let tv12 = fmap typ dom12
    enter ("new " ++ x ++ " : " ++ show tv12) $ do
      cxtenv <- ask
      let (k, delta) = cxtPushEntry dom12 (context cxtenv)
      v12 <- Traversable.mapM (up False (VGen k)) tv12
      let rho = update (environ cxtenv) x v12
      x' <- uniqueName x k 
      local (\ cxt -> cxt { context = delta
                          , renaming = Map.insert x k (renaming cxtenv)
                          , naming = Map.insert k x' (naming cxtenv)
                          , environ = rho }) (f k v12)
{-
  newVar x (tv12, dec) f = enter ("new " ++ x ++ " : " ++ show tv12) $ do
    cxtenv <- ask
    let (k, delta) = cxtPushEntry (tv12, dec) (context cxtenv)
    v12 <- Traversable.mapM (up (VGen k)) tv12
    let rho = update (environ cxtenv) x v12 
    local (\ cxt -> cxt { context = delta
                        , renaming = Map.insert x k (renaming cxtenv)
                        , environ = rho }) (f k v12)
-}
  setType k dom =
    local (\ ce -> ce { context = cxtSetType k dom (context ce) }) 

  setTypeOfName x dom cont = do
    ce <- ask
    let Just k = Map.lookup x (renaming ce)
    setType k dom cont
    
  genOfName x = do
    ce <- ask
    case Map.lookup x (renaming ce) of
      Nothing -> fail $ "internal error: variable not bound: " ++ show x
      Just k -> return k
  
  nameOfGen k = do
    ce <- ask
    case Map.lookup k (naming ce) of
      Nothing -> fail $ "internal error: no name for variable " ++ show k
      Just x -> return x

  nameTaken "" = return True
  nameTaken x = do
    ce <- ask
    st <- get
    return (Map.member x (renaming ce) || Map.member x (signature st))

  lookupGen k = do
    ce <- ask
    cxtLookupGen (context ce) k

  lookupName x = do
    ce <- ask
    cxtLookupName (context ce) (renaming ce) x

  -- does not work with shadowing!
  getContextTele = do
    ce <- ask
    let cxt = context ce   
    let ren = renaming ce
    let env = envMap $ environ ce
    let mkTBind (x,_) = (TBind x .fromOne . domain) <$> cxtLookupName cxt ren x
    mapM mkTBind env

  getLen = do
    ce <- ask
    return $ len (context ce)

  getRen = do
    ce <- ask
    return $ renaming ce

  -- since we only use getEnv during type checking, no case for Two
  -- (during equality/subtype checking, we have values)
  getEnv = do
    ce <- ask
    let (Environ rho mmeas) = environ ce
    return $ Environ (map (\ (x, One v) -> (x, v)) rho) mmeas

  applyDec dec = local (\ ce -> ce { context = cxtApplyDec dec (context ce) })
--  applyDec dec = local (\ ce -> ce { upperDecs = Map.map (compDec dec) (upperDecs ce) })

  -- resurrection sets "target" status to erased 
  -- (as opposed to setting "source" status to non-erased)
{-
  resurrect = local (\ ce -> ce { upperDecs = 
    Map.map (\ dec -> dec { erased = True }) (upperDecs ce) })
-}
{-
  resurrect = local (\ ce -> ce { context = cxtResurrect (context ce) }) 
-}


  -- PROBABLY TOO INEFFICIENT
  addRewrite rew vs cont = traceRew ("adding rewrite " ++ show rew) $
    -- add rewriting rule
    local (\ cxt -> cxt { rewrites = rew : (rewrites cxt) }) $ do
      ce <- ask
      -- normalize all types in context
      traceRewM "normalizing types in context" 
      cx' <- mapMapM (Traversable.mapM (Traversable.mapM reval)) (cxt (context ce))  -- LOOP!
      -- normalize environment 
      traceRewM "normalizing environment"
      let Environ rho mmeas = environ ce
      rho' <- mapM (\ (x,v12) -> Traversable.mapM reval v12 >>= \ v12' -> return (x, v12')) rho
      let en' = Environ rho' mmeas -- no need to rewrite in measure since only size expressions
      -- normalize given values 
      vs' <- mapM reval vs
      -- continue in updated context
      local (\ ce -> ce { context = (context ce) { cxt = cx' }
                        , environ = en' }) $ cont vs' 

  -- addPattern :: TVal -> Pattern -> (TVal -> Val -> Env -> m a) -> m a
  addPattern tv@(VPi x dom env b) p rho cont =  
       case p of
          VarP y -> new y dom $ \ xv -> do
              bv <- whnf (update env x xv) b
              cont bv xv (update rho y xv)

          SizeP z y -> newWithGen y dom $ \ j xv -> do
              bv <- whnf (update env x xv) b
              VGen k <- whnf' (Var z) 
              addSizeRel j 1 k $
                cont bv xv (update rho y xv)

          ConP co n pl -> do
              sige <- lookupSymb n
              let vc = symbTyp sige
              addPatterns vc pl rho $ \ vc' vpl rho -> do -- apply dom to pl?
                pv0 <- foldM app (vCon (coPat co) n) vpl
                pv  <- up False pv0 (typ dom)
                vb  <- whnf (update env x pv) b
                cont vb pv rho

          SuccP p2 -> do  
              addPattern (vSize `arrow` vSize) p2 rho $ \ _ vp2 rho -> do
                let pv = succSize vp2 
                vb  <- whnf (update env x pv) b
                cont vb pv rho

          ErasedP p -> addPattern tv p rho cont

-- for dot patterns, we have to do something smart, because they might
-- contain identifiers which are not yet in scope, only after adding
-- other patterns
-- the following trivial solution only works for trivial dot patterns, i.e.,
-- such that do not use yet undeclared identifiers

          DotP e -> do
              v  <- whnf rho e
              vb <- whnf (update env x v) b
              cont vb v rho -- [(x,v)]

   
  addPatterns tv [] rho cont = cont tv [] rho
  addPatterns tv (p:ps) rho cont = 
    addPattern tv p rho $ \ tv' v env -> 
      addPatterns tv' ps env $ \ tv'' vs env' -> 
        cont tv'' (v:vs) env' -- (env' ++ env)

  addSizeRel son dist father k = 
    enter -- enterTrace 
      ("adding size rel. v" ++ show son ++ " + " ++ show dist ++ " <= v" ++ show father) $ do
    local (\ cxt -> cxt { sizeRels = TSO.insert son (dist, father) (sizeRels cxt) }) k

  addBoundHyp beta@(Bound ltle (Measure mu) (Measure mu')) cont = 
    case (ltle, mu, mu') of
      (Le, _, [VInfty]) -> cont
      (Lt, _, [VInfty]) -> failure
      (ltle, [v], [v']) -> loop (if ltle==Lt then 1 else 0) v v'
      _ -> failure
    where failure = do
--            recoverFail $ "adding hypothetical constraint " ++ show beta ++ " not supported"
            assertDoc' Warning False (text "hypothetical constraint" <+> prettyTCM beta <+> text "ignored")
            cont

          loop n (VGen i) (VGen j) | n >= 0 = addSizeRel i n j cont
                                   | otherwise = addIrregularBound i j (-n) cont
          loop n (VSucc v) v' = loop (n + 1) v v'
          loop n v (VSucc v') = loop (n - 1) v v'
          loop _ _ _ = failure

          addIrregularBound i j n = local (\ ce -> ce { bounds = beta : bounds ce }) where
              v' = iterate VSucc (VGen j) !! n
              beta = Bound Le (Measure [VGen i]) (Measure [v'])
  
  sizeVarBelow son ancestor = do
    cxt <- ask
    return $ TSO.isAncestor son ancestor (sizeRels cxt)
{-  
  getSizeDiff son ancestor = do
    cxt <- ask
    return $ TSO.diff son ancestor (sizeRels cxt)
-}
  getMinSize parent = do
    cxt <- ask
    return $ TSO.height parent (sizeRels cxt)

  getSizeVarsInScope = do
    TCContext { context = delta, naming = nam } <- ask
    -- get all the size variables with positive or mixed polarity
    let fSize (i, tv12) = 
          case tv12 of
            One dom -> typ dom == VSort (SortC Size) 
            _ -> False   
    -- create a list of key (gen) and Domain pairs for the size variables
    let idl = filter fSize $ Map.toAscList (cxt delta)
    let udecs = upperDecs delta
    let fPos (i, One dom) = 
         case fromPProd (polarity (Maybe.fromJust (Map.lookup i udecs))) of
           Just p -> leqPol (polarity (decor dom)) p
           Nothing -> False
    let fName (i, _) = Maybe.fromJust $ Map.lookup i nam
    return $ map fName $ filter fPos idl


  checkingCon b = local (\ cxt -> cxt { checkingConType = b})

{-
  checkingDom = local $ \ cxt -> 
    if checkingConType cxt then cxt
     else cxt { context = cxtApplyDec (Dec False Neg) (context cxt) }
-}
  -- check domain A of (x : A) -> B
  checkingDom k = do
    b <- asks checkingConType
    if b then k else applyDec (Dec Neg) k

  setCo co = local (\ cxt -> cxt { mutualCo = co })

  installFuns co kfuns k = do
    let funt = foldl (\ m fun@(Kinded _ (TypeSig n _, _)) -> Map.insert n fun m) 
                     Map.empty 
                     kfuns    
    local (\ cxt -> cxt { mutualCo = co, funsTemplate = funt }) k

  setMeasure mu k =  do
      rho0 <- getEnv
      let rho = rho0 { envBound = Just mu }
      local (\ cxt -> cxt 
        { environ    = (environ cxt) { envBound = Just mu }
        }) k

  activateFuns k = do
      rho <- getEnv
      case (envBound rho) of
         Nothing -> k
         Just mu ->
           local (\ cxt -> cxt 
             { mutualFuns = 
                 Map.map (boundFun rho (mutualCo cxt)) (funsTemplate cxt) 
             }) k
    where boundFun :: Env -> Co -> Kinded Fun -> SigDef
          boundFun rho co (Kinded ki (TypeSig n t, (ar, cls))) = 
            FunSig co (VClos rho t) ki ar cls False undefined

{-
  activateFuns mu k = do
      rho0 <- getEnv
      let rho = rho0 { envBound = Just mu }
      local (\ cxt -> cxt 
        { environ    = (environ cxt) { envBound = Just mu }
        , mutualFuns = 
            Map.map (boundFun rho (mutualCo cxt)) (funsTemplate cxt) 
        }) k
    where boundFun :: Env -> Co -> Fun -> SigDef
          boundFun rho co (TypeSig n t, (ar, cls)) = 
            FunSig co (VClos rho t) ar cls False
 -}   

  goImpredicative = local (\ cxt -> cxt { impredicative = True })

  checkingMutual mn = local (\ cxt -> cxt { checkingMutualName = mn })


addBind :: TBind -> TypeCheck a -> TypeCheck a
addBind (TBind x dom) cont = do
  dom' <- (Traversable.mapM whnf' dom) 
  new' x dom' cont

addBinds :: Telescope -> TypeCheck a -> TypeCheck a
addBinds tel k0 = foldr addBind k0 tel

-- introduce patterns into context and environment -------------------
-- DOES NOT ETA-EXPAND VARIABLES!! -----------------------------------

introPatterns :: [Pattern] -> TVal -> ([(Pattern,Val)] -> TVal -> TypeCheck a) -> TypeCheck a
introPatterns ps tv cont =                -- Problem: NO ETA EXPANSION!
  introPatVars ps $ do                    -- first bind pattern variables
    vs <- mapM (whnf' . patternToExpr) ps -- now we can evaluate patterns
    let pvs = zip ps vs
    introPatTypes pvs tv (cont pvs)       -- now we can assign types to pvars

-- introduce variables bound in pattern into the environment
-- extend delta by generic values but do not introduce their types
-- this is to deal with dot patterns
introPatVar :: Pattern -> TypeCheck a -> TypeCheck a
introPatVar p cont = 
    case p of
      VarP n -> addName n $ \ _ -> cont 
      SizeP m n -> addName n $ \ _ -> cont 
      ConP co n pl -> introPatVars pl cont
      SuccP p -> introPatVar p cont
      ProjP{} -> cont
      DotP e -> cont
      AbsurdP -> cont
      ErasedP p -> introPatVar p cont

introPatVars :: [Pattern] -> TypeCheck a -> TypeCheck a
introPatVars [] cont = cont
introPatVars (p:ps) cont = introPatVar p $ introPatVars ps $ cont

-- if the bindings name->gen are already in the environment
-- we can now bind the gen to their types
introPatType :: (Pattern,Val) -> TVal -> (TVal -> TypeCheck a) -> TypeCheck a
introPatType (p,v) tv cont = do
  case tv of
    VGuard beta bv -> addBoundHyp beta $ introPatType (p,v) bv cont
    VApp (VDef (DefId Dat d)) vl -> 
      case p of 
        ProjP n -> cont =<< projectType tv n
        _       -> fail $ "introPatType: internal error, expected projection pattern, found " ++ show p ++ " at type " ++ show tv 
    VPi x dom env b -> do
       v <- whnfClos v
       bv <- whnf (update env x v) b
       matchPatType (p,v) (typ dom) $ cont bv
    _ -> fail $ "introPatType: internal error, expected Pi-type, found " ++ show tv 

introPatTypes :: [(Pattern,Val)] -> TVal -> (TVal -> TypeCheck a) -> TypeCheck a
introPatTypes pvs tv f = do
  case pvs of
    [] -> f tv
    (pv:pvs') -> introPatType pv tv $ \ tv' -> introPatTypes pvs' tv' f

matchPatType :: (Pattern, Val) -> TVal -> TypeCheck a -> TypeCheck a
matchPatType (p,v) av cont = 
       case (p,v) of
                                                   -- erasure does not matter!
          (VarP y, VGen k) -> setType k (defaultDomain av) $ cont

          (SizeP z y, VGen k) -> setType k (defaultDomain av) $ cont

          (ConP co n [], _) -> cont

          (ConP co n pl, VApp (VDef (DefId Con{} _)) vl) -> do
             sige <- lookupSymb n
             let vc = symbTyp sige
             introPatTypes (zip pl vl) vc $ \ _ -> cont
 
          (SuccP p2, VSucc v2) -> matchPatType (p2, v2) vSize $ cont 

          (DotP e, _) -> cont
          (AbsurdP, _) -> cont
          (ErasedP p,_) -> matchPatType (p,v) av cont
          _ -> fail $ "matchPatType: IMPOSSIBLE " ++ show (p,v)

-- signature -----------------------------------------------------
-- input to and output of the type-checker

type Signature = Map Name SigDef

-- a signature entry is either  
-- * a fun/cofun, 
-- * a defined constant, 
-- * a constructor, or 
-- * a data type id with its kind
-- they share "symbTyp", the type signature of the definition
data SigDef 
  = FunSig  { isCo          :: Co 
            , symbTyp       :: TVal 
            , symbolKind    :: Kind 
            , arity         :: Arity
            , clauses       :: [Clause] 
            , isTypeChecked :: Bool 
            , extrTyp       :: Expr   -- Fomega type
            } --type , co , clauses , whether its type checked
  | LetSig  { symbTyp       :: TVal
            , symbolKind    :: Kind 
            , definingVal   :: Val 
--            , definingExpr  :: Expr 
            , extrTyp       :: Expr   -- Fomega type
            }-- type , expr 
  | ConSig  { numPars       :: Int
            , isSized       :: Sized
            , recOccs       :: [Bool] -- which of the arguments contain rec.occs.of the (co)data type?
            , symbTyp       :: TVal   -- type
            , dataName      :: Name
            , extrTyp       :: Expr   -- Fomega type
            }   
  | DataSig { numPars       :: Int
            , positivity    :: [Pol] 
            , isSized       :: Sized 
            , isCo          :: Co
            , symbTyp       :: TVal
            , symbolKind    :: Kind 
            -- the following information is only needed for eta-expansion
            -- hence it is only provided for suitable ind.fams.
            , constructors  :: [ConstructorInfo]
            , etaExpand     :: Bool -- non-overlapping pattern inductive family
                                    -- with at least one eta-expandable constructor
            , isTuple       :: Bool -- each constructor is irrefutable
                                    -- must be (NEW: non-overlapping) pattern inductive family
                                    -- qualifies for target of corecursive fun
                                    -- NO LONGER: exactly one constructor
                                    -- NOW: at least one constructor
                                    -- can be recursive
            , extrTyp       :: Expr -- Fomega kind
{-
            , destructors   :: Maybe [Name] -- Nothing if not a record
            , isFamily      :: Bool 
-}
            } -- # parameters, positivity of parameters  , sized , co , type  
              deriving (Show)

undefinedFType n = error $ "no extracted type for " ++ n

symbKind :: SigDef -> Kind
symbKind ConSig{}  = kTerm          -- constructors are always terms
symbKind d         = symbolKind d   -- else: lookup
{- Data types can be big!!
symbKind DataSig{} = kType          -- data types are never universes
-}

emptySig = Map.empty

-- first in context, then in signature
lookupSymbTyp :: Name -> TypeCheck TVal
lookupSymbTyp n = do
  mdom <- errorToMaybe $ lookupName1 n
  case mdom of
    Just (CxtEntry dom udec) -> return (typ dom)
    Nothing -> do
      entry <- lookupSymb n
      return $ symbTyp entry

lookupSymb :: Name -> TypeCheck SigDef
lookupSymb n = do
  cxt <- ask
  case Map.lookup n (mutualFuns cxt) of
    Just k -> return $ k
    Nothing -> do
      sig <- gets signature
      lookupSig n sig 
        where
          lookupSig :: Name -> Signature -> TypeCheck SigDef
          lookupSig n sig = 
            case (Map.lookup n sig) of
              Nothing -> fail $ "identifier " ++ n ++ " not in signature "  ++ show (Map.keys sig)
              Just k -> return k
    
addSig :: Name -> SigDef -> TypeCheck ()
addSig n def = do
  st <- get
  put $ st { signature = Map.insert n def $ signature st }

modifySig :: Name -> (SigDef -> SigDef) -> TypeCheck ()
modifySig n f = do
  st <- get
  put $ st { signature = Map.adjust f n $ signature st }

setExtrTyp :: Name -> Expr -> TypeCheck ()
setExtrTyp n t = modifySig n (\ d -> d { extrTyp = t })

-- more on the type checking monad -------------------------------

initSt :: TCState
initSt = TCState emptySig emptyMetaVars emptyConstraints emptyPosGraph

initWithSig :: Signature -> TCState
initWithSig sig = TCState sig emptyMetaVars emptyConstraints emptyPosGraph

resetConstraints :: TypeCheck ()
resetConstraints = do
  st <- get
  put $ st { constraints = emptyConstraints }

vGenSuccs (VGen k)  m = (k,m)
vGenSuccs (VSucc v) m = vGenSuccs v (m+1)
vGenSuccs v m = error $ "vGenSuccs fails on " ++ Util.parens (show v) ++ " " ++ show m

retret = return . return

mkConstraint :: Val -> Val -> TypeCheck (Maybe Constraint)
mkConstraint v (VMax vs) = do
  bs <- mapM (errorToBool . leqSize' v) vs
  if any id bs then return Nothing else 
   fail $ "cannot handle constraint " ++ show v ++ " <= " ++ show (VMax vs) 
mkConstraint w@(VMax vs) v = fail $ "cannot handle constraint " ++ show w ++ " <= " ++ show v
mkConstraint (VMeta i rho n) (VMeta j rho' m) = retret $ arc (Flex i) (m-n) (Flex j)
mkConstraint (VMeta i rho n) VInfty      = retret $ arc (Flex i) 0 (Rigid (RConst Infinite))
mkConstraint (VMeta i rho n) v           = retret $ arc (Flex i) (m-n) (Rigid (RVar j))
  where (j,m) = vGenSuccs v 0
mkConstraint VInfty (VMeta i rho n)      = retret $ arc (Rigid (RConst Infinite)) 0 (Flex i)
mkConstraint v (VMeta j rho m)           = retret $ arc (Rigid (RVar i)) (m-n) (Flex j)
  where (i,n) = vGenSuccs v 0

-- addMeta k x  adds a metavariable which can refer to VGens < k
addMeta :: Ren -> MVar -> TypeCheck ()
addMeta ren i = do
  scope <- getSizeVarsInScope
  traceMetaM ("addMeta " ++ show i ++ " scope " ++ show scope)
  st <- get
  put $ st { metaVars = Map.insert i (MetaVar scope Nothing) (metaVars st)
           , constraints = NewFlex i (\ k' -> True) -- k' < k)
          -- DO NOT ADD constraints of form <= infty !!
          --               : arc (Flex i) 0 (Rigid (RConst Infinite))
                         : constraints st }
 
addLe :: LtLe -> Val -> Val -> TypeCheck ()
addLe Le v1 v2 = addLeq v1 v2
addLe Lt v1 v2 = addLeq (succSize v1) v2

addLeq :: Val -> Val -> TypeCheck ()
addLeq v1 v2 = traceMeta ("Constraint: " ++ show v1 ++ " <= " ++ show v2) $
  do mc <- mkConstraint v1 v2 
     case mc of
       Nothing -> return ()
       Just c -> do
         st <- get
         put $ st { constraints = c : constraints st }
  
solveConstraints :: TypeCheck Solution
solveConstraints = do
  cs <- gets constraints
  if null cs then return emptySolution
   else case solve cs of 
      Just subst -> -- trace ("solution" ++ show subst) $ 
                    return subst 
      Nothing    -> fail $ "size constraints " ++ show cs ++ " unsolvable" 

-- solve constraints and substitute solution into the analyzed expressions
solveAndModify :: [Expr] -> Env -> TypeCheck [Expr]
solveAndModify es rho = do
      sol <- solveConstraints
      let es' = map (subst (solToSubst sol rho)) es
      resetConstraints
      return es'

nameOf :: EnvMap -> Int -> Maybe Name
nameOf [] j = Nothing
nameOf ((x,VGen i):rho) j | i == j = Just x
nameOf (_:rho) j = nameOf rho j

sizeExprToExpr :: Env -> SizeExpr -> Expr
sizeExprToExpr rho (SizeConst Infinite) = Infty
sizeExprToExpr rho (SizeVar i n) | Just x <- nameOf (envMap rho) i = add (Var x) n
  where add e n | n <= 0 = e
                | otherwise = add (Succ e) (n-1)
sizeExprToExpr rho e@(SizeVar i n) | Nothing <- nameOf (envMap rho) i = error $ "panic: sizeExprToExpr " ++ Util.parens (show e) ++ ": variable v" ++ show i ++ " not in scope " ++ show (envMap rho)
        

maxExpr :: [Expr] -> Expr
maxExpr [] = Infty
maxExpr [e] = e
maxExpr l = if Infty `elem` l then Infty else Max l

solToSubst :: Solution -> Env -> Subst
solToSubst sol rho = Map.map (maxExpr . map (sizeExprToExpr rho)) sol


{-
solToSubst :: Solution -> Env -> Subst
solToSubst sol rho = Map.foldWithKey step Map.empty sol
  where step k (SizeVar i n) sub | Just x <- nameOf rho i =
           Map.insert k (add (Var x) n) sub
        step k (SizeConst Infinite) sub = Map.insert k Infty sub
        step _ _ sub = sub

        add e n | n <= 0 = e
                | otherwise = add (Succ e) (n-1)
-}

-- pattern to Value ----------------------------------------------

{- RETIRED 
patternToVal :: Pattern -> TypeCheck Val
patternToVal p = do
  k <- getLen
  return $ fst (p2v k p)

-- turn a pattern into a value
-- dot patterns get variables corresponding to their flexible generic value
p2v :: Int -> Pattern -> (Val,Int)
p2v k p = 
    case p of
      VarP n -> (VGen k,k+1)
      ConP co n [] -> (VCon co n,k)
      ConP co n pl -> let (vl,k') = ps2vs k pl
                      in (VApp (VCon co n) vl,k')
      SuccP p -> let (v,k') = p2v k p
                 in (VSucc v,k')   
      DotP e -> (VGen k,k+1)

ps2vs :: Int -> [Pattern] -> ([Val],Int)
ps2vs k []  = ([],k)
ps2vs k (p:pl) = let (v,k') = p2v k p
                     (vl,k'') = ps2vs k' pl
                 in
                   (v:vl,k'')
-}
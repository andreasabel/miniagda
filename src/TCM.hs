{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards, FlexibleContexts, NamedFieldPuns, DeriveFunctor, DeriveFoldable, DeriveTraversable, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}

module TCM where

import Prelude hiding (null)

import Control.Monad
import Control.Monad.State  (StateT, get, gets, put)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (ReaderT, ask, asks, local)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif
import qualified Data.Traversable as Traversable

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- import Debug.Trace

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

traceSig :: String -> a -> a
-- traceSig msg a = trace msg a
traceSig msg a = a

traceRew :: String -> a -> a
traceRew msg a = a -- trace msg a

traceRewM :: Monad m => String -> m ()
traceRewM msg = return () -- traceM msg
{-
traceRew msg a = trace msg a
traceRewM msg = traceM msg
-}

-- metavariables and constraints

traceMeta :: String -> a -> a
traceMeta msg a = a -- trace msg a

traceMetaM :: Monad m => String -> m ()
traceMetaM msg = return () -- traceM msg
{-
traceMeta msg a = trace msg a
traceMetaM msg = traceM msg
-}


-- type checking monad -----------------------------------------------

class (MonadCxt m, MonadSig m, MonadMeta m, MonadError TraceError m) =>
  MonadTCM m where


-- lists of exactly one or two elements ------------------------------

-- this would have been better implemented by just lists and a view
--   type OneOrTwo a = [a]
--   data View12 a = One a | Two a a
--   fromList12
-- then one could still get completeness of pattern matching!
-- now we have lots of boilerplate code

data OneOrTwo a = One a | Two a a deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (OneOrTwo a) where
  show (One a)   = show a
  show (Two a b) = show a ++ "||" ++ show b

name12 :: OneOrTwo Name -> Name
name12 (One n) = n
name12 (Two n1 n2)
  | null (suggestion n2) = n1
  | null (suggestion n1) = n2
  | suggestion n1 == suggestion n2 = n1
  | otherwise = fresh (suggestion n1 ++ "||" ++ suggestion n2)

{-
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
-}

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

data TCContext = TCContext
  { context   :: SemCxt
  , renaming  :: Ren       -- assigning de Bruijn Levels to names
  , naming    :: Map Int Name  -- assigning names to de Bruijn levels
--  , nameVariants :: Map Name Int -- how many variants of the name
  , environ   :: Env2
  , rewrites  :: Rewrites
  , sizeRels  :: TSO Int   -- relations of universal (rigid) size variables
                           -- collected from size patterns (x > y)
  , belowInfty:: [Int]     -- list of size variables < #
  , bounds    :: [Bound Val]  -- bound hyps that do not fit in sizeRels
  , consistencyCheck :: Bool -- ^ Do we need to check that new size relations are consistent with every valuation of the current @sizeRels@? [See ICFP 2013 paper]
  , checkingConType :: Bool  -- different PTS rules for constructor types (parametric function space!)
  , assertionHandling :: AssertionHandling -- recover from errors?
  , impredicative :: Bool       -- use impredicative PTS rules
  -- checking measured functions
  , funsTemplate :: Map Name (Kinded Fun) -- types of mutual funs with measures checking body
  , mutualFuns :: Map Name SigDef -- types of mutual funs while checking body
  , mutualCo :: Co                -- mutual block (co)recursive ?
  , mutualNames :: [Name] -- ^ The defined names of the current mutual block (and parents).
  , checkingMutualName :: Maybe DefId -- which body of a mutual block am I checking?
  , callStack :: [QName] -- ^ Used to avoid looping when going into recursive data definitions.
  , unfoldables :: Maybe Unfoldables
      -- ^ The definitions that can be unfolded currently.
      --   Like the typing context, this set can only grow as we descend into the abstract syntax tree.
      --   If @Nothing@, we unfold everything.
  }

type Unfoldables = Set Name

instance Show TCContext where
    show ce = show (environ ce) ++ "; " ++ show (context ce)

-- | Set up an initial type checking context.
emptyContext
  :: Bool       -- ^ Control unfolding of definitions?
  -> TCContext
emptyContext controlUnfolding = TCContext
  { context  = cxtEmpty
  , renaming = Map.empty
  , naming   = Map.empty
  , environ  = emptyEnv
  , rewrites = emptyRewrites
  , sizeRels = TSO.empty
  , belowInfty = []
  , bounds   = []
  , consistencyCheck = False -- initially, no consistency check, turned on when entering rhs
  , checkingConType = False
  , assertionHandling = Failure  -- default is not to ignore any errors
  , impredicative = False
  , funsTemplate = Map.empty
  , mutualFuns = Map.empty
  , mutualCo = Ind
  , mutualNames = []
  , checkingMutualName = Nothing
  , callStack = []
  , unfoldables = if controlUnfolding then Just Set.empty else Nothing
  }

-- state monad for global signature

data TCState = TCState
  { signature   :: Signature
  , metaVars    :: MetaVars
  , constraints :: Constraints
  , positivityGraph :: PositivityGraph
  -- , dots        :: Dots -- UNUSED
  }

type MetaVars = Map MVar MetaVar

emptyMetaVars :: MetaVars
emptyMetaVars = Map.empty

type MScope = [Name] -- ^ names of size variables which are in scope of mvar
data MetaVar = MetaVar
  { mscope   :: MScope
  , solution :: Maybe Val
  }

type PosConstrnt = Constrnt PPoly DefId ()
type PositivityGraph = [PosConstrnt]

emptyPosGraph :: PositivityGraph
emptyPosGraph = []

-- type TypeCheck = StateT TCState (ReaderT TCContext (CallStackT String IO))
type TypeCheck = StateT TCState (ReaderT TCContext (ExceptT TraceError IO))

-- TODO: make TypeCheck a newtype to support custoom MonadFail
-- instance MonadFail TypeCheck where
--   fail msg = throwErrorMsg $ unwords [ "internal error:", msg ]

instance MonadAssert TypeCheck where
  assert b s = do
    h <- asks assertionHandling
    assert' h b s
  newAssertionHandling h = local ( \ ce -> ce { assertionHandling = h })

{- NOT NEEDED

-- | Dotted constructors (the top one in the pattern).
type Dots = [(Dotted,Pattern)]

emptyDots = []

class LensDots a where
  getDots :: a -> Dots
  setDots :: Dots -> a -> a
  setDots = mapDots . const
  mapDots :: (Dots -> Dots) -> a -> a
  mapDots f a = setDots (f (getDots a)) a

instance LensDots TCState where
  getDots = dots
  setDots d st = st { dots = d }

newDotted :: Pattern -> TypeCheck Dotted
newDotted p = do
  d <- mkDotted True
  modify $ mapDots $ ((d,p):)
  return d

clearDots :: TypeCheck ()
clearDots = modify $ setDots emptyDots

openDots :: TypeCheck [Pattern]
openDots = map snd . filter (isDotted . fst) <$> gets dots
-}

-- rewriting rules -----------------------------------------------

data Rewrite  = Rewrite { lhs :: Val,  rhs :: Val }
type Rewrites = [Rewrite]

emptyRewrites :: Rewrites
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

cxtEmpty :: SemCxt
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
  where bot = error $ "IMPOSSIBLE: name " ++ show x ++ " is not bound to any type"

-- only defined for single bindings
cxtSetType :: Int -> Domain -> SemCxt -> SemCxt
cxtSetType k dom delta =
  delta { cxt  = Map.insert k (One dom) (cxt delta)
        -- upperDecs need not be updated
        }

-- | Version of 'Map.lookup' that throws 'TraceError'.
lookupM :: (MonadError TraceError m, Show k, Ord k) => k -> Map k v -> m v
lookupM k m = maybe (throwErrorMsg $ "lookupM: unbound key " ++ show k) return $ Map.lookup k m

cxtLookupGen :: MonadError TraceError m => SemCxt -> Int -> m CxtEntry
cxtLookupGen delta k = do
  dom12 <- lookupM k (cxt delta)
  udec  <- lookupM k (upperDecs delta)
  return $ CxtEntry dom12 udec

cxtLookupName :: MonadError TraceError m => SemCxt -> Ren -> Name -> m CxtEntry
cxtLookupName delta ren x = do
  i <- lookupM x ren
  cxtLookupGen delta i

-- apply decoration, possibly resurrecting (see Pfenning, LICS 2001)
-- and changing polarities (see Abel, MSCS 2008)
cxtApplyDec :: Dec -> SemCxt -> SemCxt
cxtApplyDec dec delta = delta { upperDecs = Map.map (compDec dec) (upperDecs delta) }
-- cxtApplyDec dec delta =  delta { decs = Map.map (fmap $ invCompDec dec) (decs delta) }

-- manipulating the context ------------------------------------------

{-
-- | Size decrements in bounded quantification do not count for termination
data LamPi
  = LamBind -- ^ add a lambda binding to the context
  | PiBind  -- ^ add a pi binding to the context
-}

class (Functor m, Monad m) => MonadCxt m where
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
--  nameTaken  :: Name -> m Bool
  uniqueName :: Name -> Int -> m Name
  uniqueName x _ = return x -- $ freshen x -- TODO!  now freshen causes problems in extraction
{-
  uniqueName x k = ifM (nameTaken x) (return $ show x ++ "~" ++ show k) (return x)
-}
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
  addBelowInfty :: Int -> m a -> m a
  addBoundHyp :: Bound Val -> m a -> m a
  isBelowInfty :: Int -> m Bool
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

  -- | Is the given name unfoldable
  canUnfold :: Name -> m Bool
  canUnfold x = maybe True (x `Set.member`) <$> askUnfoldables
  askUnfoldables :: m (Maybe Unfoldables)
  modifyUnfoldablesM :: (Unfoldables -> m Unfoldables) -> m a -> m a

dontCare :: a
dontCare = error "Internal error: tried to retrieve unassigned type of variable"

instance MonadCxt TypeCheck where

  newIrr x = local (\ ce -> ce { environ = update (environ ce) x (One VIrr) })

  -- UPDATE to 2?
  addName x f = enter ("new " ++ show x ++ " : _") $ do
    cxtenv <- ask
    let (k, delta) = cxtPushGen x (context cxtenv)
    let v = VGen k
    let rho = update (environ cxtenv) x (One v)
    x' <- uniqueName x k
    local (\ cxt -> cxt { context = delta
                        , renaming = Map.insert x k (renaming cxtenv)
                        , naming = Map.insert k x' (naming cxt)
                        , environ = rho }) (f v)


  newVar x dom12@(One (Domain (VBelow ltle v) ki dec)) f = do
    enter ("new " ++ show x ++ " " ++ show ltle ++ " " ++ show v) $ do
      cxtenv <- ask
      let (k, delta) = cxtPushEntry (One (Domain vSize kSize dec)) (context cxtenv)
      let xv  = VGen k
      let v12 = One xv
      let rho = update (environ cxtenv) x v12
      let beta = Bound ltle (Measure [xv]) (Measure [v])
      x' <- uniqueName x k
      local (\ cxt -> cxt { context = delta
                          , renaming = Map.insert x k (renaming cxtenv)
                          , naming = Map.insert k x' (naming cxtenv)
                          , environ = rho }) $
        addBoundHyp beta $ (f k v12)


  newVar x dom12 f = do
    let tv12 = fmap typ dom12
    enter ("new " ++ show x ++ " : " ++ show tv12) $ do
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
      Nothing -> throwErrorMsg $ "internal error: variable not bound: " ++ show x
      Just k -> return k

  nameOfGen k = do
    ce <- ask
    case Map.lookup k (naming ce) of
      Nothing -> return $ fresh $ "error_unnamed_gen" ++ show k
       -- throwErrorMsg $ "internal error: no name for variable " ++ show k
      Just x -> return x

{-
  nameTaken "" = return True
  nameTaken x = do
    ce <- ask
    st <- get
    return (Map.member x (renaming ce) || Map.member x (signature st))
-}

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
  addPattern tv@(VQuant Pi x dom fv) p rho cont =
       case p of
          VarP y -> underAbs y dom fv $ \ _ xv bv -> do
              cont bv xv (update rho y xv)

          SizeP e y -> underAbs y dom fv $ \ j xv bv -> do
              ve <- whnf' e
              addBoundHyp (Bound Lt (Measure [xv]) (Measure [ve])) $
                cont bv xv (update rho y xv)
{-
          SizeP z y -> newWithGen y dom $ \ j xv -> do
              bv <- whnf (update env x xv) b
              VGen k <- whnf' (Var z)
              addSizeRel j 1 k $
                cont bv xv (update rho y xv)
-}
          ConP pi n pl -> do
              sige <- lookupSymbQ n
              vc <- conLType n (typ dom)
              addPatterns vc pl rho $ \ vc' vpl rho -> do -- apply dom to pl?
                pv0 <- mkConVal notDotted (coPat pi) n vpl vc
                pv  <- up False pv0 (typ dom)
                vb  <- app fv pv
                cont vb pv rho
{-
          ConP pi n pl -> do
              sige <- lookupSymb n
              let vc = symbTyp sige
              addPatterns vc pl rho $ \ vc' vpl rho -> do -- apply dom to pl?
                pv0 <- foldM app (vCon (coPat pi) n) vpl
                pv  <- up False pv0 (typ dom)
                vb  <- whnf (update env x pv) b
                cont vb pv rho
-}
          SuccP p2 -> do
              addPattern (vSize `arrow` vSize) p2 rho $ \ _ vp2 rho -> do
                let pv = succSize vp2
                vb  <- app fv pv
                cont vb pv rho

          ErasedP p -> addPattern tv p rho cont

-- for dot patterns, we have to do something smart, because they might
-- contain identifiers which are not yet in scope, only after adding
-- other patterns
-- the following trivial solution only works for trivial dot patterns, i.e.,
-- such that do not use yet undeclared identifiers

          DotP e -> do
              v  <- whnf rho e
              vb <- app fv v
              cont vb v rho -- [(x,v)]


  addPatterns tv [] rho cont = cont tv [] rho
  addPatterns tv (p:ps) rho cont =
    addPattern tv p rho $ \ tv' v env ->
      addPatterns tv' ps env $ \ tv'' vs env' ->
        cont tv'' (v:vs) env' -- (env' ++ env)

  addSizeRel son dist father k = do
    let s = "v" ++ show son ++ " + " ++ show dist ++ " <= v" ++ show father
    enter -- enterTrace
      ("adding size rel. " ++ s) $ do
    let modBI belowInfty = if father `elem` belowInfty || dist > 0 then son : belowInfty else belowInfty
    whenM (asks consistencyCheck `andLazy` do
           TSO.increasesHeight son (dist, father) <$> asks sizeRels) $ do
      recoverFail $ "cannot add hypothesis " ++ s ++ " because it is not satisfyable under all possible valuations of the current hypotheses"
    -- if the new son is an ancestor of the father, we are cyclic
    whenJustM (TSO.isAncestor father son <$> asks sizeRels) $ \ n -> -- n steps from father up to son
      when (dist > - n) $ -- still ok if dist == n == 0, otherwise fail
        recoverFail$ "cannot add hypothesis " ++ s ++ " because it makes the set of hyptheses unsatisfiable"
    local (\ cxt -> cxt
      { sizeRels = TSO.insert son (dist, father) (sizeRels cxt)
      , belowInfty = modBI (belowInfty cxt)
      }) k

  addBelowInfty i = local $ \ cxt -> cxt { belowInfty = i : belowInfty cxt }

  addBoundHyp beta@(Bound ltle (Measure mu) (Measure mu')) cont =
    case (ltle, mu, mu') of
      (Le, _, [VInfty]) -> cont
--      (Lt, _, [VInfty]) -> failure  -- handle j < #
      (ltle, [v], [v']) -> loop (if ltle==Lt then 1 else 0) v v'
      _ -> failure
    where failure = do
--            recoverFail $ "adding hypothetical constraint " ++ show beta ++ " not supported"
            assertDoc' Warning False (text "hypothetical constraint" <+> prettyTCM beta <+> text "ignored")
            cont

          loop n (VGen i) VInfty = addBelowInfty i cont
          loop n (VGen i) (VGen j) | n >= 0 = addSizeRel i n j cont
                                   | otherwise = addIrregularBound i j (-n) cont
          loop n (VSucc v) v' = loop (n + 1) v v'
          loop n v (VSucc v') = loop (n - 1) v v'
          loop _ _ _ = failure

          addIrregularBound i j n = local (\ ce -> ce { bounds = beta : bounds ce }) where
              v' = iterate VSucc (VGen j) !! n
              beta = Bound Le (Measure [VGen i]) (Measure [v'])

  isBelowInfty i = (i `elem`) <$> asks belowInfty

{-
  isBelowInfty i = do
    belowInfty <- asks belowInfty
    if (i `elem` belowInfty) then return True else do
      tso <- asks sizeRels
      loop $ parents i tso where
        loop [] = return False
        loop [(_,j)] = return $ j `elem` belowInfty
        loop (x:xs)  = loop xs
-}

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
            One dom -> isVSize $ typ dom
            _ -> -- trace ("not a size variable " ++ show i ++ " : " ++ show tv12) $
                   False
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

  -- install functions for checking function clauses
  -- ==> use internal names
  installFuns co kfuns k = do
    let funt = foldl (\ m fun@(Kinded _ (Fun (TypeSig n _) n' _ _unfolds _)) -> Map.insert n fun m)
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
    where
      boundFun :: Env -> Co -> Kinded Fun -> SigDef
      boundFun rho co (Kinded ki (Fun (TypeSig n t) n' ar unfolds cls)) =
        FunSig co (VClos rho t) ki ar unfolds cls False undefined

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

  askUnfoldables = asks unfoldables

  modifyUnfoldablesM f cont = do
    unf <- traverse f =<< askUnfoldables
    local (\ cxt -> cxt{ unfoldables = unf }) cont

-- | Go into the codomain of a Pi-type or open an abstraction.
underAbs  :: Name -> Domain -> FVal -> (Int -> Val -> Val -> TypeCheck a) -> TypeCheck a
underAbs x dom fv cont = newWithGen x dom $ \ i xv -> cont i xv =<< app fv xv

-- | Do not check consistency preservation of context.
underAbs_  :: Name -> Domain -> FVal -> (Int -> Val -> Val -> TypeCheck a) -> TypeCheck a
underAbs_ x dom fv cont = noConsistencyChecking $ underAbs x dom fv cont

noConsistencyChecking :: TypeCheck a -> TypeCheck a
noConsistencyChecking = local $ \ cxt -> cxt { consistencyCheck = False }

-- | No eta, no hypotheses.  First returned val is a @VGen i@.
underAbs' :: Name -> FVal -> (Val -> Val -> TypeCheck a) -> TypeCheck a
underAbs' x fv cont = addName x $ \ xv -> cont xv =<< app fv xv

-- addBind :: MonadTCM m => TBind -> m a -> m a
addBind :: TBind -> TypeCheck a -> TypeCheck a
addBind (TBind x dom) cont = do
  dom' <- (Traversable.mapM whnf' dom)
  new' x dom' cont

addBinds :: Telescope -> TypeCheck a -> TypeCheck a
addBinds tel k0 = foldr addBind k0 $ telescope tel

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
      PairP p1 p2 -> introPatVars [p1,p2] cont
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
    VApp (VDef (DefId DatK d)) vl ->
      case p of
        ProjP n -> cont =<< projectType tv n VIrr -- no record value here
        _       -> throwErrorMsg $ "introPatType: internal error, expected projection pattern, found " ++ show p ++ " at type " ++ show tv
    VQuant Pi x dom fv -> do
       v  <- whnfClos v
       matchPatType (p,v) dom . cont =<< app fv v
    _ -> throwErrorMsg $ "introPatType: internal error, expected Pi-type, found " ++ show tv

introPatTypes :: [(Pattern,Val)] -> TVal -> (TVal -> TypeCheck a) -> TypeCheck a
introPatTypes pvs tv f = do
  case pvs of
    [] -> f tv
    (pv:pvs') -> introPatType pv tv $ \ tv' -> introPatTypes pvs' tv' f

matchPatType :: (Pattern, Val) -> Domain -> TypeCheck a -> TypeCheck a
matchPatType (p,v) dom cont =
       case (p,v) of
                                                   -- erasure does not matter!
          (VarP y, VGen k) -> setType k dom $ cont

          (SizeP z y, VGen k) -> setType k dom $ cont

          (ConP co n [], _) -> cont

          (ConP co n pl, VApp (VDef (DefId ConK{} _)) vl) -> do
{-
             sige <- lookupSymb n
             let vc = symbTyp sige
-}
             vc <- conType n =<< force (typ dom)
             introPatTypes (zip pl vl) vc $ \ _ -> cont

          (SuccP p2, VSucc v2) -> matchPatType (p2, v2) (defaultDomain vSize) $ cont

          (PairP p1 p2, VPair v1 v2) -> do
             av <- force (typ dom)
             case av of
               VQuant Sigma x dom1@(Domain av1 ki dec) fv -> do
                 matchPatType (p1,v1) dom1 $ do
                   bv <- app fv v1
                   matchPatType (p2,v2) (Domain bv ki dec) cont
               _ -> throwErrorMsg $ "matchPatType: IMPOSSIBLE " ++ show p ++ "  :  " ++ show dom

          (DotP e, _) -> cont
          (AbsurdP, _) -> cont
          (ErasedP p,_) -> matchPatType (p,v) dom cont
          _ -> throwErrorMsg $ "matchPatType: IMPOSSIBLE " ++ show (p,v)


-- Signature -----------------------------------------------------

-- input to and output of the type-checker

type Signature = Map QName SigDef

-- a signature entry is either
-- - a fun/cofun,
-- - a defined constant,
-- - a constructor, or
-- - a data type id with its kind
-- they share "symbTyp", the type signature of the definition
data SigDef
  = FunSig  { isCo          :: Co
            , symbTyp       :: TVal
            , symbolKind    :: Kind
            , arity         :: Arity
            , unfolds       :: Unfolds
                -- ^ Definitions that may have been unfolded when checking the clauses.
            , clauses       :: [Clause]
            , isTypeChecked :: Bool
            , extrTyp       :: Expr   -- ^ Fomega type.
            }
  | LetSig  { symbTyp       :: TVal
            , symbolKind    :: Kind
            , definingVal   :: Val
            , unfolds       :: Unfolds
--            , definingExpr  :: Expr
            , extrTyp       :: Expr   -- ^ Fomega type.
            }
  | PatSig  { patVars       :: [Name]
            , definingPat   :: Pattern
            , definingVal   :: Val
            }
  | ConSig  { conPars       :: ConPars
              -- ^ Parameter patterns and no. of variable they bind.
              --   @Nothing@ if old-style parameters.
            , lhsTyp        :: LHSType
              -- ^ LHS type of constructor for pattern matching, e.g.
   -- rhs @cons : [A : Set] [i : Size]         -> A -> List A i -> List A $i@
   -- lhs @cons : [A : Set] [i : Size] [j < i] -> A -> List A j -> List A i@
   -- @Name@ is the name of the size parameter.
            , recOccs       :: [Bool]
              -- ^ @True@ if argument contains rec.occs.of the (co)data type?
            , symbTyp       :: TVal   -- ^ (RHS) type, includs parameter tel.
            , dataName      :: Name   -- ^ Its datatype.
            , dataPars      :: Int    -- ^ No. of parameters of its datatype.
            , extrTyp       :: Expr   -- ^ Fomega type.
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

-- | Parameter patterns and no. of variables they bind.
type ConPars = Maybe ([Name], [Pattern])

-- | LHS type plus name of size index.
type LHSType = Maybe (Name, TVal)

-- |
unfoldsOfDef :: SigDef -> Unfolds
unfoldsOfDef = unfolds
  -- TODO:

isEmptyData :: QName -> TypeCheck Bool
isEmptyData n = do
  sig <- lookupSymbQ n
  case sig of
    DataSig { constructors } -> return $ null constructors
    _ -> throwErrorMsg $ "internal error: isEmptyData " ++ show n ++ ": name of data type expected"

isUnitData :: QName -> TypeCheck Bool
isUnitData n = do
  sig <- lookupSymbQ n
  case sig of
    DataSig { constructors = [c], isTuple } -> return $
      isTuple && null (cFields c) && cPatFam c == (LinearPatterns, [])
    DataSig { constructors } -> return False
    _ -> throwErrorMsg $ "internal error: isUnitData " ++ show n ++ ": name of data type expected"


undefinedFType :: QName -> Expr
undefinedFType n = Irr
-- undefinedFType n = error $ "no extracted type for " ++ show n

symbKind :: SigDef -> Kind
symbKind ConSig{}  = kTerm          -- constructors are always terms
symbKind d         = symbolKind d   -- else: lookup
{- Data types can be big!!
symbKind DataSig{} = kType          -- data types are never universes
-}

emptySig :: Signature
emptySig = Map.empty

-- Handling constructor types  ------------------------------------------

data DataView
  = Data Name [Clos]
  | NoData

-- | Check if type @tv@ is a datatype @D vs@.
dataView :: TVal -> TypeCheck DataView
dataView tv = do
  tv <- force tv
  case tv of
{- 2012-01-31 EVIL, LEADS TO UNBOUND VARS:
    VQuant Pi x dom env b         -> do
      new x dom $ \ xv -> dataView =<< whnf (update env x xv) b
-}
    VApp (VDef (DefId DatK n)) vs -> return $ Data (unqual n) vs
    VSing v dv                    -> dataView =<< whnfClos dv
    _                             -> return $ NoData

-- | Disambiguate possibly overloaded constructor @c@ at given type @tv@.
disambigCon ::  QName -> TVal -> TypeCheck QName
disambigCon c tv =
  case c of
    Qual{}  -> return c
    QName n -> do
      dv <- dataView tv
      case dv of
        Data d _ -> return $ Qual d n
        _ -> throwErrorMsg $ "cannot resolve constructor " ++ show n

-- | @conType c tv@ returns the type of constructor @c@ at datatype @tv@
--   with parameters instantiated.
conType :: QName -> TVal -> TypeCheck TVal
conType c tv = do
  c <- disambigCon c tv
  ConSig { conPars, symbTyp, dataName, dataPars } <- lookupSymbQ c
  instConType c conPars symbTyp dataName dataPars tv

-- | Get LHS type of constructor.
--
--   Constructors or sized data types internally have a lhs type
--   that differs from its rhs type.  E.g.,
--   rhs @suc : [i : Size] -> Nat i -> Nat $i@
--   lhs @suc : [i : Size] [j < i] -> Nat j -> Nat i@.
--   In the lhs type, @i@ turns into an additional parameter.
conLType :: QName -> TVal -> TypeCheck TVal
conLType c tv = do
  c <- disambigCon c tv
  ConSig { conPars, lhsTyp, symbTyp, dataName, dataPars } <- lookupSymbQ c
  case lhsTyp of
    Nothing        -> instConType c conPars symbTyp dataName dataPars tv
    Just (x, lTyp) -> instConType c (fmap (inc x) conPars) lTyp dataName (dataPars+1) tv
  where inc x (xs, ps) = (xs ++ [x], ps ++ [VarP x])

-- | Instantiate type of constructor to parameters obtained from
--   the data type.
--
--   @instConType c n symbTyp dataName tv@
--   instantiates type @symbTyp@ of constructor @c@ with first @n@ arguments
--   that @dataName@ is applied to in @tv@.
--   @@
--      instConType c n ((x1:A1..xn:An) -> B) d (d v1..vn ws) = B[vs/xs]
--   @@
instConType :: QName -> ConPars -> TVal -> Name -> Int -> TVal -> TypeCheck TVal
instConType c conPars symbTyp dataName dataPars tv =
  instConLType' c conPars symbTyp Nothing (Just dataName) dataPars tv
{-
instConType c numPars symbTyp dataName tv = do
  dv <- dataView tv
  case dv of
    NoData    -> failDoc (text ("conType " ++ show c ++ ": expected")
                   <+> prettyTCM tv <+> text "to be a data type")
    Data d vs -> do
      unless (d == dataName) $ throwErrorMsg $ "expected constructor of datatype " ++ show d ++ ", but found one of datatype " ++ show dataName
      let (pars, inds) = splitAt numPars vs
      unless (length pars == numPars) $
        failDoc (text ("conType " ++ show c ++ ": expected")
                   <+> prettyTCM tv
                   <+> text ("to be a data type applied to all of its " ++
                     show numPars ++ " parameters"))
      piApps symbTyp pars
-}

-- | Get correct lhs type for constructor pattern.
--
--   @instConLType c numPars symbTyp Nothing isFlex tv@ behaves like
--   @instConLType c numPars symbType _ tv@.
--
--   But if the data types is sized and the constructor has a lhs type,
--   @instConLType c numPars symbTyp (Just ltv) isFlex tv@
--   uses the lhs type @ltv@ unless the variable instantiated for
--   the size argument is flexible (because then it wants to be
--   unified with the successor pattern of the rhs type.
instConLType :: QName -> ConPars -> TVal -> LHSType -> (Val -> Bool) -> Int -> TVal -> TypeCheck TVal
instConLType c conPars rhsTyp lhsTyp isFlex dataPars dataTyp =
  instConLType' c conPars rhsTyp (fmap (,isFlex) lhsTyp) Nothing dataPars dataTyp

-- | The common pattern behind @instConType@ and @instConLType@.
instConLType' :: QName -> ConPars -> TVal -> Maybe ((Name, TVal), Val -> Bool) -> Maybe Name -> Int -> TVal -> TypeCheck TVal
instConLType' c conPars symbTyp isSized md dataPars tv =
  enter ("instConLType'") $ do
  let failure = failDoc (text ("conType " ++ show c ++ ": expected")
                   <+> prettyTCM tv
                   <+> text ("to be a data type applied to all of its " ++
                     show dataPars ++ " parameters"))
  dv <- dataView tv
  case dv of
    NoData    -> failDoc (text ("conType " ++ show c ++ ": expected")
                   <+> prettyTCM tv <+> text "to be a data type")
    Data d vs -> do
      whenJust md $ \ d' ->
        unless (d == d') $ throwErrorMsg $ "expected constructor of datatype " ++ show d ++ ", but found one of datatype " ++ show d'
      -- whenJust conPars $ throwErrorMsg $ "NYI: constructor with pattern parameters"
      let (pars, inds) = splitAt dataPars vs
      unless (length pars == dataPars) failure
      case (isSized, inds) of
        (Just _, []) -> failure
        -- if size index not flexible, use lhs type
        (Just ((x,ltv), isFlex), sizeInd:_) | not (isFlex sizeInd) ->
          continue d [x] ltv (pars ++ [sizeInd])
        -- otherwise, use rhs type
        _ -> continue d [] symbTyp pars
  where
    continue d ys tv pars = case conPars of
      Nothing      -> piApps tv pars
      Just (xs, ps) -> do
        let failure = failDoc $ sep
              [ text "instConType:"
              , text "cannot match parameters" <+> prettyList (map prettyTCM pars)
              , text "against patterns" <+> prettyList (map prettyTCM ps)
              , text "when instantiating type" <+> prettyTCM tv
              , text ("of constructor " ++ show c)
              ]
        -- clear dots here:
        mst <- nonLinMatchList' True True (emptyEnv, []) ps pars =<< lookupSymbTyp d
        case mst of
          Nothing  -> failure
          Just (Environ{ envMap = env0 }, psub) -> do
            let env = env0 ++ [ (x, VGen i) | (i, VarP x) <- psub ]
            -- if length env /= length xs then failure else do
            vs <- forM (xs ++ ys) $ \ x -> maybe failure return $ lookup x env
            piApps tv vs
{-
        menv <- matchList emptyEnv ps pars
        case menv of
          Nothing  -> failure
          Just Environ{ envMap = env } -> if length env /= length xs then failure else do
            vs <- forM (xs ++ ys) $ \ x -> maybe failure return $ lookup x env
            piApps tv vs
-}

{-
      case isSized of
        Nothing  -> piApps symbTyp pars
        Just ltv -> do
          when (null inds) failure
          let sizeInd = head inds
          if isFlex sizeInd then piApps symbTyp pars else piApps ltv (pars ++ [sizeInd])
-}

-- Signature specification -------------------------------------------

class MonadCxt m => MonadSig m where
  lookupSymbTypQ :: QName -> m TVal
  lookupSymbQ    :: QName -> m SigDef
  addSigQ        :: QName -> SigDef -> m ()
  modifySigQ     :: QName -> (SigDef -> SigDef) -> m ()
  setExtrTypQ    :: QName -> Expr -> m ()

  lookupSymbTyp  :: Name -> m TVal
  lookupSymbTyp  = lookupSymbTypQ . QName

  lookupSymb     :: Name -> m SigDef
  lookupSymb     = lookupSymbQ . QName

  addSig         :: Name -> SigDef -> m ()
  addSig         = addSigQ . QName

  modifySig      :: Name -> (SigDef -> SigDef) -> m ()
  modifySig      = modifySigQ . QName

  setExtrTyp     :: Name -> Expr -> m ()
  setExtrTyp     = setExtrTypQ . QName

  -- | Register symbols that can be unfolded, plus their dependencies (transitively).
  addUnfolds     :: Unfolds -> m a -> m a
  addUnfolds     = modifyUnfoldablesM . loop
    where
    loop :: Unfolds -> Unfoldables -> m Unfoldables
    loop []     acc = return acc
    loop (n:ns) acc
        | n `Set.member` acc
                    = loop ns acc
        | otherwise = do
            ns' <- unfolds <$> lookupSymb n
            loop (ns' ++ ns) (n `Set.insert` acc)
  -- addUnfolds unf = modifyUnfoldablesM $ \ acc -> loop acc [unf]
  --   where
  --   loop acc = \case
  --     []          -> return acc
  --     [] : ls     -> loop acc ls
  --     (n:ns) : ls
  --       | n `Set.member` acc -> loop acc (ns : ls)
  --       | otherwise -> do
  --           ns' <- unfolds <$> lookupSymb n
  --           loop (n `Set.insert` acc) (ns' : ns : ls)

-- Signature implementation ------------------------------------------

instance MonadSig TypeCheck where

  -- first in context, then in signature
  -- lookupSymbTyp :: Name -> TypeCheck TVal
  lookupSymbTyp n = do
    mdom <- errorToMaybe $ lookupName1 n
    case mdom of
      Just (CxtEntry dom udec) -> return (typ dom)
      Nothing -> symbTyp <$> lookupSymb n

  lookupSymbTypQ (QName n) = lookupSymbTyp n
  lookupSymbTypQ n@Qual{}  = symbTyp <$> lookupSymbQ n

  -- lookupSymb :: Name -> TypeCheck SigDef
  lookupSymb n = do
    cxt <- ask
    case Map.lookup n (mutualFuns cxt) of
      Just k  -> return $ k
      Nothing -> lookupSymbInSig (QName n)

  lookupSymbQ (QName n) = lookupSymb n
  lookupSymbQ n@Qual{}  = lookupSymbInSig n

  -- addSig :: Name -> SigDef -> TypeCheck ()
  addSigQ n def = traceSig ("addSig: " ++ show n ++ " is bound to " ++ show def) $ do
    st <- get
    put $ st { signature = Map.insert n def $ signature st }

  -- modifySig :: Name -> (SigDef -> SigDef) -> TypeCheck ()
  modifySigQ n f = do
    st <- get
    put $ st { signature = Map.adjust f n $ signature st }

  -- setExtrTyp :: Name -> Expr -> TypeCheck ()
  setExtrTypQ n t = modifySigQ n (\ d -> d { extrTyp = t })

lookupSymbInSig :: QName -> TypeCheck SigDef
lookupSymbInSig n = lookupSig n =<< gets signature
    where
      -- lookupSig :: Name -> Signature -> TypeCheck SigDef
      lookupSig n sig =
        case (Map.lookup n sig) of
          Nothing -> throwErrorMsg $ "identifier " ++ show n ++ " not in signature "  ++ show (Map.keys sig)
          Just k -> return k


-- more on the type checking monad -------------------------------

initSt :: TCState
initSt = TCState emptySig emptyMetaVars emptyConstraints emptyPosGraph -- emptyDots

initWithSig :: Signature -> TCState
initWithSig sig = initSt { signature = sig }

-- Meta-variable and constraint handling specification ---------------

class Monad m => MonadMeta m where
  resetConstraints :: m ()
  mkConstraint     :: Val -> Val -> m (Maybe Constraint)
  addMeta          :: Ren -> MVar -> m ()
  addLeq           :: Val -> Val -> m ()

  addLe            :: LtLe -> Val -> Val -> m ()
  addLe Le v1 v2 = addLeq v1 v2
  addLe Lt v1 v2 = addLeq (succSize v1) v2 -- broken for #

  solveConstraints :: m Solution

  -- solve constraints and substitute solution into the analyzed expressions
  solveAndModify   :: [Expr] -> Env -> m [Expr]
  solveAndModify es rho = do
        sol <- solveConstraints
        let es' = map (subst (solToSubst sol rho)) es
        resetConstraints
        return es'

-- Constraints implementation ----------------------------------------

instance MonadMeta TypeCheck where

  --resetConstraints :: TypeCheck ()
  resetConstraints = do
    st <- get
    put $ st { constraints = emptyConstraints }

  -- mkConstraint :: Val -> Val -> TypeCheck (Maybe Constraint)
  mkConstraint v (VMax vs) = do
    bs <- mapM (errorToBool . leqSize' v) vs
    if any id bs then return Nothing else
     throwErrorMsg $ "cannot handle constraint " ++ show v ++ " <= " ++ show (VMax vs)
  mkConstraint w@(VMax vs) v = throwErrorMsg $ "cannot handle constraint " ++ show w ++ " <= " ++ show v
  mkConstraint (VMeta i rho n) (VMeta j rho' m) = return $ Just $ arc (Flex i) (m-n) (Flex j)
  mkConstraint (VMeta i rho n) VInfty      = return $ Just $ arc (Flex i) 0 (Rigid (RConst Infinite))
  mkConstraint (VMeta i rho n) v           = return $ Just $ arc (Flex i) (m-n) (Rigid (RVar j))
    where (j,m) = vGenSuccs v 0
  mkConstraint VInfty (VMeta i rho n)      = return $ Just $ arc (Rigid (RConst Infinite)) 0 (Flex i)
  mkConstraint v (VMeta j rho m)           = return $ Just $ arc (Rigid (RVar i)) (m-n) (Flex j)
    where (i,n) = vGenSuccs v 0
  mkConstraint v1 v2 = throwErrorMsg $ "mkConstraint undefined for " ++ show (v1,v2)

  -- addMeta k x  adds a metavariable which can refer to VGens < k
  -- addMeta :: Ren -> MVar -> TypeCheck ()
  addMeta ren i = do
    scope <- getSizeVarsInScope
    traceMetaM ("addMeta " ++ show i ++ " scope " ++ show scope)
    st <- get
    put $ st { metaVars = Map.insert i (MetaVar scope Nothing) (metaVars st)
             , constraints = NewFlex i (\ k' -> True) -- k' < k)
            -- DO NOT ADD constraints of form <= infty !!
            --               : arc (Flex i) 0 (Rigid (RConst Infinite))
                           : constraints st }

  -- addLeq :: Val -> Val -> TypeCheck ()
  addLeq v1 v2 = traceMeta ("Constraint: " ++ show v1 ++ " <= " ++ show v2) $
    do mc <- mkConstraint v1 v2
       case mc of
         Nothing -> return ()
         Just c -> do
           st <- get
           put $ st { constraints = c : constraints st }

  -- solveConstraints :: TypeCheck Solution
  solveConstraints = do
    cs <- gets constraints
    if null cs then return emptySolution
     else case solve cs of
        Just subst -> traceMeta ("solution" ++ show subst) $
                      return subst
        Nothing    -> throwErrorMsg $ "size constraints " ++ show cs ++ " unsolvable"


nameOf :: EnvMap -> Int -> Maybe Name
nameOf [] j = Nothing
nameOf ((x,VGen i):rho) j | i == j = Just x
nameOf (_:rho) j = nameOf rho j

vGenSuccs :: Val -> Int -> (Int, Int)
vGenSuccs (VGen k)  m = (k,m)
vGenSuccs (VSucc v) m = vGenSuccs v (m+1)
vGenSuccs v m = error $ "vGenSuccs fails on " ++ Util.parens (show v) ++ " " ++ show m

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

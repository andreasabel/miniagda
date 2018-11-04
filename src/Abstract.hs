-- Some optimizations (-O) destroy the expected behavior of unsafePerformIO
-- So, special options are needed, plus NOINLINE for the affected functions.
{-# OPTIONS -fno-cse -fno-full-laziness #-}

{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
  GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable,
  NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Abstract where

import Prelude hiding (showList, map, concat, foldl, pi, null, (<>))

import Control.Applicative hiding (empty)
import Control.Monad.Writer (Writer, tell, All(..))
import Control.Monad.Trans

import Data.Monoid hiding ((<>))
import Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable as Foldable
import Data.Traversable as Traversable
import Data.Unique

import Data.List (map)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace
import Data.IORef
import System.IO.Unsafe

import Text.PrettyPrint as PP

import Collection (Collection)
import qualified Collection as Coll
import Polarity as Pol
import TreeShapedOrder (TSO)
import qualified TreeShapedOrder as TSO
import Util hiding (parens, brackets)
import qualified Util
import {-# SOURCE #-} Value (TeleVal)

-- * Names carry a name suggestion and a unique identifier

-- | Each Name is classified as "User", "EtaAlias", or "Quote".
data WhatName
  = UserName
  | EtaAliasName -- ^ a name for the eta-expanded name of a definition
  | QuoteName
    deriving (Eq, Ord, Show)

data Name = Name
  { suggestion :: String    -- ^ suggestion for printing the name.
  , what       :: WhatName
  , uid        :: Unique -- !Unique
  }

-- | Names are compared according to their UID.
instance Eq Name where
  x == x' = uid x == uid x'

instance Ord Name where
  compare x x' = compare (uid x) (uid x')

instance Show Name where
  show (Name n _ u) = n -- n ++ "`" ++ show (hashUnique u `mod` 13)

-- | @fresh s@ generates a new name with 'suggestion' @s@.
--
--   To a void a monad here, we use imperative features (@unsafePerformIO@).
fresh :: String -> Name
fresh n = Name n UserName $ unsafePerformIO newUnique
{-# NOINLINE fresh #-}

freshen :: Name -> Name
freshen n = fresh (suggestion n)

-- | A non-unique empty name.  Use only inconstant functions!
noName :: Name
noName = fresh ""

-- | Check whether name is @""@.
emptyName :: Name -> Bool
emptyName n = null (suggestion n)

nonEmptyName :: Name -> String -> Name
nonEmptyName n s | emptyName n = n { suggestion = s }
                 | otherwise   = n

-- | Get the first non-empty name from a non-empty list of names.
bestName :: [Name] -> Name
bestName [n]    = n
bestName (n:ns)
  | emptyName n = bestName ns
  | otherwise   = n

-- temporary hack for reification

iAmNotUnique :: Unique
iAmNotUnique = unsafePerformIO newUnique
{-# NOINLINE iAmNotUnique #-}

unsafeName :: String -> Name
unsafeName s = Name s QuoteName iAmNotUnique

-- | External reference to recursive function (outside of the body).
mkExtName :: Name -> Name
mkExtName n = Name (suggestion n) EtaAliasName $ unsafePerformIO newUnique
-- mkExtName n = "_" ++ n
{-# NOINLINE mkExtName #-}

mkExtRef  n = letdef (mkExtName n)

isEtaAlias :: Name -> Bool
isEtaAlias n = what n == EtaAliasName

-- | Internal name for compiler-generated stuff.
internal :: Name -> Name
internal n = freshen n
-- internal n = "__" ++ n
-- internal names are prefixed by a double underscore (not legal concrete syntax)

-- | Convert a dot pattern into an identifier which should not look too confusing.
spaceToUnderscore = List.map (\ c -> if c==' ' then '_' else c)
{-
exprToName e = spaceToUnderscore $ show e
patToName p  = spaceToUnderscore $ show p
-}

-- | Qualified name.
data QName
  = Qual  { qual :: Name, name :: Name }
  | QName { name :: Name }
  deriving (Eq, Ord)

instance Show QName where
  show (Qual m n) = show m ++ "." ++ show n
  show (QName n)  = show n

-- | An unqualified name is an instance of a qualified name.
nameInstanceOf (QName n) (Qual _ n') = n == n'
nameInstanceOf n         n'          = n == n'

-- | Fails if qualified name.
unqual (QName n) = n
unqual n         = error $ "Abstract.unqual: " ++ show n

data Sized = Sized | NotSized
             deriving (Eq,Ord,Show)

data Co = Ind
        | CoInd
          deriving (Eq,Ord,Show)

showFun :: Co -> String
showFun Ind   = "fun"
showFun CoInd = "cofun"

data LtLe = Lt | Le deriving (Eq,Ord)

instance Show LtLe where
  show Lt = "<"
  show Le = "<="

-- decoration of Pi-types --------------------------------------------

-- 1. whether argument is irrelevant / its polarity
-- further possibilities:
-- 2. hidden

data Decoration pos
    = Dec { thePolarity :: pos }
    | Hidden
  deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

polarity :: Polarity pol => Decoration pol -> pol
polarity Hidden    = hidden
polarity (Dec pol) = pol

instance Polarity a => Polarity (Decoration a) where
  erased        = erased . polarity
  compose  p p' = Dec $ compose (polarity p) (polarity p')
  neutral       = Dec neutral
  promote       = Dec . promote . polarity
  demote        = Dec . demote . polarity
  hidden        = Hidden

type Dec = Decoration Pol
type UDec = Decoration PProd

class LensPol a where
  getPol :: a -> Pol
  setPol :: Pol -> a -> a
  setPol = mapPol . const
  mapPol :: (Pol -> Pol) -> a -> a
  mapPol f a = setPol (f (getPol a)) a

instance LensPol Dec where
  getPol = polarity
  setPol p Hidden = Hidden
  setPol p dec    = dec { thePolarity = p }

udec :: Dec -> UDec
udec = fmap pprod

irrelevantDec = Dec Pol.Const
paramDec = Dec Param
defaultDec = Dec defaultPol
-- defaultDec = paramDec -- TODO: Dec { polarity = Rec }
defaultUpperDec = Dec $ pprod SPos
  -- a variable may not be erased and its polarity must be below SPos
-- notErased = Dec False
-- resurrectDec d = d { erased = False }

-- | Composing with 'neutralDec' should do nothing.
neutralDec = Dec SPos

coDomainDec :: Dec -> Dec
coDomainDec Hidden = Dec Param -- REDUNDANT
coDomainDec dec
    | polarity dec == Pol.Const = Dec Param
    | otherwise                 = Dec Rec

-- compDec dec dec'
-- composition of decoration, used when type checking arguments
-- of functions decorated with dec
compDec :: Dec -> UDec -> UDec
compDec dec udec = compose (fmap pprod dec) udec

{-
instance Show pos => Show (Decoration pos) where
    show p =
      (if erased p then Util.brackets else Util.parens) $ show $ polarity p
-}


{- OLD CODE
data Decoration pos = Dec { erased :: Bool, polarity :: pos }
           deriving (Eq, Ord, Functor, Foldable, Traversable)

type Dec = Decoration Pol
type UDec = Decoration PProd

irrelevantDec = Dec { erased = True, polarity = Pol.Const }
defaultDec = Dec { erased = False, polarity = Rec }
defaultUpperDec = Dec { erased = False, polarity = pprod SPos }
  -- a variable may not be erased and its polarity must be below SPos
-- notErased = Dec False
resurrectDec d = d { erased = False }

{- RETIRED
-- invCompDec dec dec'
-- inverse composition of decoration, used when type checking arguments
-- of functions decorated with dec
invCompDec :: Dec -> Dec -> Dec
invCompDec (Dec er pol) (Dec er' pol') = Dec
  (if er then False else er')
  (invComp pol pol')
-}

-- compDec dec dec'
-- composition of decoration, used when type checking arguments
-- of functions decorated with dec
compDec :: Dec -> UDec -> UDec
compDec (Dec er pol) (Dec er' pol') = Dec
  (er || er')      -- erasing once is sufficient
  (polProd (pprod pol) pol')

instance Show pos => Show (Decoration pos) where
    show (Dec erased polarity) =
      (if erased then Util.brackets else Util.parens) $ show polarity
-}

-- size expressions --------------------------------------------------

class HasPred a where
  predecessor :: a -> Maybe a

instance HasPred Expr where
  predecessor (Succ e) = Just e
  predecessor _ = Nothing

sizeSuccE :: Expr -> Expr
sizeSuccE Infty = Infty
sizeSuccE e     = Succ e

minSizeE :: Expr -> Expr -> Expr
minSizeE Infty e2 = e2
minSizeE e1 Infty = e1
minSizeE Zero  e2 = Zero
minSizeE e1 Zero  = Zero
minSizeE (Succ e1) (Succ e2) = Succ (minSizeE e1 e2)
minSizeE e1 e2 = error $ "minSizeE " ++ (Util.parens $ show e1) ++ " " ++ (Util.parens $ show e2)

maxSizeE :: Expr -> Expr -> Expr
maxSizeE Infty e2 = Infty
maxSizeE e1 Infty = Infty
maxSizeE Zero  e2 = e2
maxSizeE e1 Zero  = e1
maxSizeE (Succ e1) (Succ e2) = Succ (maxSizeE e1 e2)
maxSizeE e1 e2 = Max [e1, e2]
-- maxSizeE e1 e2 = error $ "maxSizeE " ++ (Util.parens $ show e1) ++ " " ++ (Util.parens $ show e2)

flattenMax :: Expr -> [Expr] -> [Expr]
flattenMax Infty          acc = [Infty]
flattenMax Zero           acc = acc
flattenMax (Max [])       acc = acc
flattenMax (Max (e : es)) acc = flattenMax e $ flattenMax (Max es) acc
flattenMax e              acc = e : acc

-- smart constructor for MAX
maxE :: [Expr] -> Expr
maxE es = Max $ foldr flattenMax [] es

sizeVarsToInfty :: Expr -> Expr
sizeVarsToInfty Zero = Zero
sizeVarsToInfty (Succ e) = sizeSuccE (sizeVarsToInfty e)
sizeVarsToInfty _ = Infty

leqSizeE :: Expr -> Expr -> Bool
leqSizeE Zero e  = True
leqSizeE e Zero  = False
leqSizeE e Infty = True
leqSizeE (Succ e) (Succ e') = leqSizeE e e'
leqSizeE Infty e = False

-- plus :: Expr -> Expr -> Expr

-- sorts -------------------------------------------------------------

data Class
  = Tm      -- sort of terms, only needed for erasure
--  | Ty    -- use Set 0!  -- sort of type(constructor)s, only needed for erasure
--  | Ki      -- sort of kinds  -- use Set 0 ... for mor precision
  | Size    -- sort of sizes
  | TSize   -- sort of Size
  -- | Type    -- no longer used
    deriving (Eq, Ord, Show)

predClass :: Class -> Class
-- predClass Ty    = Tm
predClass TSize = Size
predClass Tm    = Tm
predClass Size  = Size

data Sort a
  = SortC Class -- sort constant (Size, TSize)
  | Set a       -- Set 0 = CoSet #, Set 1 = Type 1, Set 2 = Type 2, ...
  | CoSet a     -- sized version of Set
    deriving (Eq, Ord, Functor, Foldable, Traversable)

{-
instance Show a => Show (Sort a) where
  show (SortC c) = show c
  show (Set a)   = "Set " ++ show a
  show (CoSet a) = "CoSet " ++ show a
-}

instance Show (Sort Expr) where
  show (SortC c) = show c
  show (Set Zero) = "Set"
  show (CoSet Infty) = "Set"
  show (Set e) = Util.parens $ ("Set " ++ show e)
  show (CoSet e) = Util.parens $ ("CoSet " ++ show e)

topSort :: Sort Expr
topSort = Set Infty

-- | The expression representing the type Size.
tSize :: Expr
tSize = Sort (SortC Size)

-- | Checking whether an expression represents type Size.
isSize :: Expr -> Bool
isSize (Sort (SortC Size)) = True
isSize (Below Le Infty)    = True
isSize _                   = False

predSort :: Sort Expr -> Sort Expr
predSort (SortC  c)     = SortC (predClass c)
predSort (CoSet  e)     = SortC Tm
predSort (Set Zero)     = SortC Tm
predSort (Set (Succ e)) = Set e
predSort (Set Infty)    = Set Infty
predSort s@(Set Var{})  = s
predSort s = error $ "internal error: predSort " ++ show s

-- only for sorts appearing in kinds:

succSort :: Sort Expr -> Sort Expr
succSort (SortC Size) = SortC TSize
succSort (SortC Tm)   = Set Zero
succSort (Set e)      = Set (sizeSuccE e)

minSort :: Sort Expr -> Sort Expr -> Sort Expr
minSort (SortC Tm) (Set e) = SortC Tm
minSort (Set e) (SortC Tm) = SortC Tm
minSort (Set e) (Set e') = Set (minSizeE e e')
-- minSort (SortC c) (SortC c') | c == c' = SortC c
minSort (SortC c) (SortC c') = SortC $ minClass c c'
minSort s s' = error $ "minSort (" ++ show s ++ ") (" ++ show s' ++ ") not implemented"

-- 2012-01-21: that should not be necessary, but to move on...
minClass :: Class -> Class -> Class
minClass Tm c = Tm
minClass c Tm = Tm
minClass Size c = Size
minClass c Size = Size
minClass TSize TSize = TSize
maxClass :: Class -> Class -> Class

maxClass Tm c = c
maxClass c Tm = c
maxClass Size c = c
maxClass c Size = c
maxClass TSize TSize = TSize

maxSort :: Sort Expr -> Sort Expr -> Sort Expr
maxSort (SortC Tm) (Set e) = Set e
maxSort (Set e) (SortC Tm) = Set e
maxSort (Set e) (Set e') = Set (maxSizeE e e')
-- maxSort (SortC c) (SortC c') | c == c' = SortC c
maxSort (SortC c) (SortC c') = SortC $ maxClass c c'
maxSort s s' = error $ "maxSort (" ++ show s ++ ") (" ++ show s' ++ ") not implemented"

{-
leSort :: Sort -> Sort -> Bool
leSort _ Type = True
leSort Type _ = False
leSort s s'   = s == s'
-}

-- s `irrSortFor` s' if a variable of kind s cannot compuationally
-- contribute to produce a value of kind s'
irrSortFor :: Sort Expr -> Sort Expr -> Bool
irrSortFor (SortC Tm) _          = False -- terms matter for terms and everything
irrSortFor _          (SortC Tm) = True  -- nothing else can be eliminated into a term
irrSortFor (SortC Size) _        = False -- sizes matter for everything but terms
irrSortFor _        (SortC Size) = True  -- nothing else can be eliminated into a size
irrSortFor (SortC TSize) _        = False -- sizes matter for everything but terms
irrSortFor _        (SortC TSize) = True  -- nothing else can be eliminated into a size
irrSortFor (Set e) (Set e')      = not $ leqSizeE e e'

-- kinds -------------------------------------------------------------

-- kinds classify expressions into terms, types, universes, ...
-- since the analysis is not precise, we give an interval of classes

data Kind
  = Kind { lowerKind :: Sort Expr , upperKind :: Sort Expr }
  | NoKind   -- absurd clauses, neutral wrt. union
  | AnyKind  -- not yet classified, neutral wrt. intersection
    deriving (Eq, Ord)

--defaultKind = Kind (SortC Tm) topSort -- no classification, could be anything
defaultKind = AnyKind

preciseKind s = Kind s s
kSize   = preciseKind (SortC Size)
kTSize  = preciseKind (SortC TSize)
kTerm   = preciseKind (SortC Tm)
kType   = preciseKind (Set Zero)
kUniv e = preciseKind (Set (Succ (sizeVarsToInfty e))) -- used in TypeChecker

instance Show Kind where
  show NoKind = "()"
  show AnyKind = "?"
--  show k | k == defaultKind = "?"
  show (Kind kl ku) | kl == ku = show kl
  show (Kind kl ku) = show kl ++ ".." ++ show ku

-- print kind in four letters
prettyKind :: Kind -> String
prettyKind NoKind                       = "none"
prettyKind AnyKind                      = "anyk"
-- prettyKind k | k == defaultKind         = "anyk"
prettyKind (Kind _ (SortC Tm))          = "term"
prettyKind (Kind _ (SortC Size))        = "size"
prettyKind k | k == kType               = "type"
prettyKind (Kind (Set (Succ Zero)) _)   = "univ"
prettyKind (Kind (Set Zero) _)          = "ty-u"
prettyKind (Kind (SortC Tm) (Set Zero)) = "tmty"
prettyKind k                            = "mixk"

-- if D : T and T has kind ki, then D has kind dataKind ki
dataKind :: Kind -> Kind
dataKind (Kind _ (Set (Succ e))) = Kind (Set Zero) (Set e)

-- in (x : A) -> B, if x : A and A has kind ki, then x has kind argKind ki
argKind :: Kind -> Kind
argKind NoKind = NoKind
argKind AnyKind = AnyKind
argKind (Kind s s') = Kind (predSort s) (predSort s')

-- if e : A and A has kind ki, then e has kind predKind ki
predKind :: Kind -> Kind
predKind NoKind = NoKind
predKind AnyKind = AnyKind
-- predecessors in the kind hierarchy
predKind ki@(Kind _ (SortC Size))  = error $ "predKind " ++ show ki
predKind (Kind _ (SortC TSize)) = kSize
-- proper types are only inhabited by terms
predKind (Kind _ (Set Zero)) = kTerm
-- proper universes are inhabited by types and universes
predKind (Kind (Set (Succ e)) s) = Kind (Set Zero) (predSort s)
-- something which is a type or a universe can be inhabited by a term
predKind (Kind _ s) = Kind (SortC Tm) (predSort s)

succKind :: Kind -> Kind
succKind AnyKind = AnyKind
succKind (Kind _ (SortC Tm)) = kType
succKind (Kind _ (SortC Size)) = kTSize
succKind (Kind s _) = Kind (succSort s) (Set Infty) -- no upper bound

-- partial operation!
intersectKind :: Kind -> Kind -> Kind
intersectKind NoKind ki = ki -- NoKind means here "intersection is not happening"
intersectKind ki NoKind = ki
intersectKind AnyKind ki = ki
intersectKind ki AnyKind = ki
intersectKind (Kind x1 x2) (Kind y1 y2) =
  Kind (maxSort x1 y1) (minSort x2 y2)

unionKind :: Kind -> Kind -> Kind
unionKind ki1 ki2 = -- trace (show ki1 ++ " `unionKind` " ++ show ki2) $
  case (ki1,ki2) of
    (NoKind, ki) -> ki
    (ki, NoKind) -> ki
    (AnyKind, ki) -> AnyKind
    (ki, AnyKind) -> AnyKind
    (Kind x1 x2, Kind y1 y2) ->
      Kind (minSort x1 y1) (maxSort x2 y2)

-- ki `irrelevantFor` ki' if an argument of kind ki cannot
-- computationally contribute to a result of kind ki'
irrelevantFor :: Kind -> Kind -> Bool
irrelevantFor NoKind _ = False -- do not make a statement if there is no info
irrelevantFor _ NoKind = False
irrelevantFor AnyKind _ = False
irrelevantFor _ AnyKind = False
irrelevantFor (Kind s _) (Kind _ s') = irrSortFor s s'
-- worst case szenario: the least kind of the argument is still
-- irrelevant for the biggest kind of the result

data Kinded a = Kinded { kindOf :: Kind, valueOf :: a }
                deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Kinded a) where
--  show (Kinded ki a) | ki == defaultKind = show a
  show (Kinded ki a) = show a ++ "::" ++ show ki

-- function domains --------------------------------------------------

data Dom a = Domain { typ :: a, kind :: Kind, decor :: Dec }
             deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Dom a) where
    show (Domain ty ki dec) = show dec ++ show ty ++ "::" ++ show ki

defaultDomain a = Domain a defaultKind defaultDec
domFromKinded (Kinded ki t) = Domain t ki defaultDec
defaultIrrDom a = Domain a defaultKind irrelevantDec

sizeDomain :: Dec -> Dom Expr
sizeDomain dec = Domain tSize kTSize dec

belowDomain :: Dec -> LtLe -> Expr -> Dom Expr
belowDomain dec ltle e = Domain (Below ltle e) kTSize dec

class LensDec a where
  getDec :: a -> Dec
  setDec :: Dec -> a -> a
  setDec d = mapDec $ const d
  mapDec :: (Dec -> Dec) -> a -> a
  mapDec f a = setDec (f $ getDec a) a

instance LensDec (Dom a) where
  getDec = decor
  setDec d dom = dom { decor = d }

instance LensPol (Dom a) where
  getPol = getPol . getDec
  mapPol = mapDec . mapPol

{-
instance Functor Dom where
  fmap f dom = dom { typ = f (typ dom) }

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
instance Traversable Dom where
  traverse f dom = (\ ty -> dom { typ = ty }) <$> f (typ dom)
-}

-- identifiers -------------------------------------------------------

-- |
data ConK
  = Cons    -- ^ a constructor
  | CoCons  -- ^ a coconstructor
  | DefPat  -- ^ a defined pattern
    deriving (Eq, Ord, Show)

data IdKind
  = DatK       -- ^ data/codata
  | ConK ConK  -- ^ constructor (ind/coind/defined)
  | FunK       -- ^ fun/cofun
  | LetK       -- ^ let definition
    deriving (Eq, Ord)

instance Show IdKind where
    show DatK   = "data"
    show ConK{} = "con"
    show FunK   = "fun"
    show LetK   = "let"

conKind (ConK _) = True
conKind _        = False

coToConK Ind = Cons
coToConK CoInd = CoCons

data DefId = DefId { idKind :: IdKind, idName :: QName }
           deriving (Eq, Ord)

instance Show DefId where
    show d = show (idName d) -- ++ "@" ++ show (idKind d)

type MVar = Int -- metavariables are numbered

-- typed bindings in Pi, LLet, Telescope -----------------------------

data TBinding a = TBind
  { boundName :: Name        -- ^ @emptyName@ if non-dependent.
  , boundDom  :: Dom a       -- ^ @x : T@ or @i < j@.
  }
  | TMeasure (Measure Expr)  -- ^ Measure @|m|@.
  | TBound   (Bound Expr)    -- ^ Constraint @|m| <(=) |m'|@.
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

type LBind = TBinding (Maybe Type)
type TBind = TBinding Type

noBind :: Dom a -> TBinding a
noBind = TBind (fresh "")

boundType :: TBind -> Type
boundType = typ . boundDom

instance LensDec (TBinding a) where
  getDec = getDec . boundDom
  mapDec f (TBind x dom) = TBind x (dom { decor = f (decor dom) })
  mapDec f tb = tb

mapDecM :: (Applicative m) => (Dec -> m Dec) -> TBind -> m TBind
mapDecM f tb@TBind{} = flip setDec tb <$> f (getDec tb)
mapDecM f tb         = pure tb

-- measures ----------------------------------------------------------

newtype Measure a = Measure { measure :: [a] }    -- mu
    deriving (Eq,Ord,Functor,Foldable,Traversable)

instance Show a => Show (Measure a) where
    show (Measure l) = "|" ++ showList "," show l ++ "|"

succMeasure :: (a -> a) -> Measure a -> Measure a
succMeasure succ mu = maybe (error "cannot take successor of empty measure") id $ applyLastM (Just . succ) mu

{-
succMeasure succ (Measure mu) = Measure (succMeas mu)
  where succMeas []     = error "cannot take successor of empty measure"
        succMeas [e]    = [succ e]
        succMeas (e:es) = e : succMeas es
-}

applyLastM :: (a -> Maybe a) -> Measure a -> Maybe (Measure a)
applyLastM f (Measure mu) = Measure <$> loop mu
  where loop []     = fail "empty measure"
        loop [e]    = (:[]) <$> f e
        loop (e:es) = (e:)  <$> loop es

instance HasPred a => HasPred (Measure a) where
  predecessor mu = applyLastM predecessor mu

data Bound a = Bound { ltle :: LtLe, leftBound :: Measure a, rightBound :: Measure a }  -- mu < mur  of mu <= mu'
    deriving (Eq,Ord,Functor,Foldable,Traversable)

instance Show a => Show (Bound a) where
  show (Bound Lt mu1 mu2) = show mu1 ++ " < " ++ show mu2
  show (Bound Le mu1 mu2) = show mu1 ++ " <= " ++ show mu2

{-
instance (HasPred a, Show a) => Show (Bound a) where
    show (Bound mu1 mu2) = case predecessor mu2 of
      Just mu2 -> show mu1 ++ " <= " ++ show mu2
      Nothing  -> show mu1 ++ " < " ++ show mu2
-}

-- TODO: properly implement bounds mu <= mu' such that mu <= # is
-- represented correctly

-- tagging expressions -----------------------------------------------

data Tag
  = Erased -- ^ Expression will be erased.
  | Cast   -- ^ Expression will need to be casted.
  deriving (Eq,Ord,Show)

type Tags = [Tag]

inTags :: Tag -> Tags -> Bool
inTags = elem

noTags = []

data Tagged a = Tagged { tags :: Tags , unTag :: a }
  deriving (Eq,Ord,Functor,Foldable,Traversable)

instance Show a => Show (Tagged a) where
  show (Tagged tags a) =
   bracketsIf (Erased `inTags` tags) $
     showCast (Cast `inTags` tags) $
       show  a

showCast :: Bool -> String -> String
showCast True  s = "'cast" ++ Util.parens s
showCast False s = s

instance Pretty a => Pretty (Tagged a) where
  prettyPrec k (Tagged []   a) = prettyPrec k a
  prettyPrec _ (Tagged tags a) =
    prettyErased (Erased `inTags` tags) $
      prettyCast (Cast `inTags` tags) $
        pretty a

prettyErased True  doc = brackets doc
prettyErased False doc = doc

prettyCast True  doc = text "'cast" <> PP.parens doc
prettyCast False doc = doc

-- expressions -------------------------------------------------------

data Expr
  = Sort (Sort Expr)   -- ^ @Size@ @Set@ @CoSet@
  -- sizes
  | Zero
  | Succ Expr
  | Infty
  | Max [Expr]   -- ^ (list has at least 2 elements)
  | Plus [Expr]  -- ^ (list has at least 2 elements)
  -- identifiers
  | Meta MVar    -- ^ meta-variable
  | Var Name     -- ^ variables are named
  | Def DefId    -- ^ identifiers in the signature
{-
  | Con Co Name [Expr] -- constructors applied to arguments
  | Def Name     -- fun/cofun ?
  | Let Name     -- definition (non-recursive)
-}
  -- dependently typed lambda calculus
  | Record RecInfo [(Name,Expr)] -- ^ record { p1 = e1; ...; pn = en }
  | Proj PrePost Name            -- ^ proj _  or  _ .proj
  | Pair Expr Expr
  | Case Expr (Maybe Type) [Clause]
    -- ^ Type is @Nothing@ in input, @Just@ after t.c.
  | LLet LBind Telescope Expr Expr
    -- ^ @let [x : A] = t in u@, @let [x] tel = t in u@
    --   after t.c. @Telescope@ is empty (fused into @LBind@)
  | App Expr Expr
  | Lam Dec Name Expr
  | Quant PiSigma TBind Expr
  | Sing Expr Expr  -- <t : A> singleton type
  -- instead of bounded quantification, a type for subsets
  -- use as @Pi/Sigma (TBind ... (Below ltle a)) b@
  | Below LtLe Expr                     -- ^ <(a : Size) or <=(a : Size)
  -- for extraction
  | Ann (Tagged Expr) -- ^ annotated expr, e.g. with Erased tag
  | Irr -- ^ for instance the term correponding to the absurd pattern
    deriving (Eq,Ord)

data PrePost = Pre | Post deriving (Eq, Ord, Show)
data PiSigma = Pi | Sigma deriving (Eq, Ord)

instance Show PiSigma where
  show Pi    = "->"
  show Sigma = "&"

-- | Optional constructor name of a record value.
data RecInfo
  = AnonRec                           -- ^ anonymous record
  | NamedRec { recConK :: ConK
             , recConName :: QName    -- ^ record constructor
             , recNamedFields :: Bool -- ^ print field names?
             , recDottedRef :: Dotted -- ^ coming from dotted constructor (unconfirmed)
             }
  deriving (Eq, Ord)

newtype Dotted = Dotted { dottedRef :: IORef Bool }

instance Eq   Dotted where x == y = True
instance Ord  Dotted where x <= y = True
instance Show Dotted where show d = fwhen (isDotted d) ("un" ++) "confirmed"

-- A bit of imperative programming

mkDotted :: MonadIO m => Bool -> m Dotted
mkDotted b = liftIO $ Dotted <$> newIORef b

-- default value, shared over all instances
{-# NOINLINE notDotted #-}
notDotted :: Dotted
notDotted = unsafePerformIO $ mkDotted False

isDotted :: Dotted -> Bool
isDotted = unsafePerformIO . readIORef . dottedRef

clearDotted :: MonadIO m => Dotted -> m ()
clearDotted d | isDotted d = liftIO $ do
      -- putStrLn ("clearing a dot")
      writeIORef (dottedRef d) False
  | otherwise = return ()

alignDotted :: MonadIO m => Dotted -> Dotted -> m ()
alignDotted d1 d2 = case (isDotted d1, isDotted d2) of
  (True, False) -> clearDotted d1
  (False, True) -> clearDotted d2
  _             -> return ()

recDotted :: RecInfo -> Bool
recDotted NamedRec{recDottedRef} = isDotted recDottedRef
recDotted AnonRec = False

instance Show RecInfo where
  show AnonRec              = ""
  show ri@NamedRec{recConName} = (if recDotted ri then "." else "") ++ show recConName

-- * smart constructors

-- | Create a universal binding.  Fuse hidden bindings.
pi :: TBind -> Expr -> Expr
pi = piSig Pi

piSig :: PiSigma -> TBind -> Expr -> Expr
piSig = Quant
{-
piSig piSig ta e =
  case ta of
    ta@TBind{ boundDom = Domain{ decor = Hidden }} ->
      case e of
        Quant piSig' tel tb c | piSig == piSig'
          -> Quant piSig (Telescope $ ta : telescope tel) tb c
        _ -> error $ "lone hidden binding" ++ show ta
    _ -> Quant piSig emptyTel ta e
-}

proj :: Expr -> PrePost -> Name -> Expr
proj e Pre n  = App (Proj Pre n) e
proj e Post n = App e (Proj Post n)

-- | Non-dependent function type.
funType a b = Quant Pi (noBind a) b

erasedExpr e = Ann (Tagged [Erased] e)
castExpr   e = Ann (Tagged [Cast]   e)

succView :: Expr -> (Int, Expr)
succView (Succ e) = inc (succView e) where inc (n, e) = (n+1, e)
succView e = (0, e)

-- Clauses and patterns ----------------------------------------------

data Clause = Clause
  { clTele     :: TeleVal      -- top-level telescope of type values for PVars
  , clPatterns :: [Pattern]
  , clExpr     :: Maybe Expr   -- Nothing if absurd clause
  } deriving (Eq,Ord,Show)

-- clause = Clause (error "internal error: no telescope in clause before typechecking!")
clause = Clause [] -- empty clTele

data PatternInfo = PatternInfo
  { coPat          :: ConK    -- (co)constructor
  , irrefutablePat :: Bool    -- constructor of a record (UNUSED)
  , dottedPat      :: Bool
  } deriving (Eq,Ord,Show)

type Pattern = Pat Expr

-- | Patterns parametrized by type of dot patterns.
data Pat e
  = VarP Name                      -- ^ x
  | ConP PatternInfo QName [Pat e] -- ^ (c ps) and (.c ps)
  | SuccP (Pat e)                  -- ^ ($ p)
  | SizeP e Name                   -- ^ (x > y) (# > y) ($x > y)
  | PairP (Pat e) (Pat e)          -- ^ (p, p')
  | ProjP Name                     -- ^ .proj
  | DotP e                         -- ^ .e
  | AbsurdP                        -- ^ ()
  | ErasedP (Pat e)                -- ^ pattern which got erased
  | UnusableP (Pat e)
{- ^ a pattern which results from matching a coinductive type and
the corresponding size index is not in the coinductive result type of
the function.  Such a pattern is not usable for termination
checking. -}
{-
             | IrrefutableP (Pat e) -- pattern made from record constructors
                                    -- can be matched by applying destructors
  NOT GOOD ENOUGH.  Irrefutable constructors might be mixed with others, e.g.

    pair x refl

  The whole pattern is not irrefutable, but still you want the pair destructed
  lazily by projections.
-}
--  | IrrP -- pattern which got erased
               deriving (Eq,Ord)

{-
-- which pattern shapes are irrefutable?
-- only ConP and SuccP might be refutable
irrefutable :: Pattern -> Bool
irrefutable ConP{} = False
irrefutable SuccP{} = False
irrefutable VarP{}         = True
irrefutable SizeP{}        = True
irrefutable IrrefutableP{} = True
irrefutable DotP{}         = True
irrefutable AbsurdP{}      = True
irrefutable ErasedP{}      = True
-}

type Case = (Pattern,Expr)

type Subst = Map MVar Expr

con co n = Def $ DefId (ConK co) n
-- con co n = Con co n []
fun n    = Def $ DefId FunK n
dat n    = Def $ DefId DatK n
letdef n = Def $ DefId LetK $ QName n

type SpineView = (Expr, [Expr])

-- collect applications to expose head
spineView :: Expr -> SpineView
spineView = aux []
  where aux sp (App f e) = aux (e:sp) f
        aux sp e = (e, sp)

test_spineView = spineView ((Var x `App` Var y) `App` Var z)
  where x = fresh "x"
        y = fresh "y"
        z = fresh "z"
{-
  where x = Name "x" $ unsafePerformIO newUnique
        y = Name "y" $ unsafePerformIO newUnique
        z = Name "z" $ unsafePerformIO newUnique
-}

{-
-- sort expressions
set  = Sort Set
size = Sort Size
-}

isErasedExpr :: Expr -> (Bool, Expr)
isErasedExpr (Ann (Tagged tags e)) =
  let (b, e') = isErasedExpr e
  in  (b || Erased `inTags` tags, e')
isErasedExpr e = (False, e)

type Extr = Expr -- extracted expressions
type EType = Type -- extracted types

-- declarations --------------------------------------------------

data Declaration
  = DataDecl Name Sized Co [Pol] Telescope Type [Constructor] [Name] -- data/codata
  | RecordDecl Name Telescope Type Constructor [Name] -- record
  | MutualFunDecl Bool Co [Fun]     -- mutual fun block / mutual cofun block, bool for measured
  | FunDecl Co Fun  -- fun, possibly inside MutualDecl
  | LetDecl Bool Name Telescope (Maybe Type) Expr
      -- ^ Bool for eval.  After t.c., tel. is empty and type is Just.
  | PatternDecl Name [Name] Pattern
  | MutualDecl Bool [Declaration]  -- mutual data/fun block, bool for measured
  | OverrideDecl Override [Declaration]    -- expect/ignore some type error
    deriving (Eq,Ord,Show)

data Override
  = Fail            -- ^ expect an error, ignore block
  | Check           -- ^ expect no error, still ignore block
  | TrustMe         -- ^ ignore recoverable errors
  | Impredicative   -- ^ use impredicativity for these declarations
    deriving (Eq,Ord,Show)

data TySig a = TypeSig { namePart :: Name, typePart :: a }
               deriving (Eq,Ord,Show,Functor)
type TypeSig = TySig Type

type Type = Expr

-- | Constructor declaration.  Top-level scope (independent of data pars).
data Constructor = Constructor
 { ctorName :: QName       -- ^ Name of the constructor.
 , ctorPars :: ParamPats   -- ^ Constructor patterns (if new style params).
 , ctorType :: Type        -- ^ Constructor type (@fields -> target@).
 } deriving (Eq, Ord, Show)

type ParamPats = Maybe (Telescope, [Pattern])

newtype Telescope = Telescope { telescope :: [TBind] }
  deriving (Eq, Ord, Show, Size, Null)

emptyTel = Telescope []

data Arity = Arity
  { fullArity    :: Int        -- ^ arity of the function
  , isProjection :: Maybe Int  -- ^ projection? then number of parameters
  } deriving (Eq, Ord, Show)

data Fun = Fun
  { funTypeSig :: TypeSig      -- ^ internal name and type
  , funExtName :: Name         -- ^ external name (for associated eta-expanded fun)
  , funArity   :: Arity
  , funClauses :: [Clause]
  } deriving (Eq, Ord, Show)

{-
letToFun :: TypeSig -> Expr -> Fun
letToFun ts e = (ts, (0, [Clause [] $ Just e]))
-}

-- extracted declarations --------------------------------------------

type EDeclaration = Declaration
type EClause      = Clause
type EPattern     = Pattern
type EConstructor = Constructor
type ETypeSig     = TypeSig
type EFun         = Fun
type ETelescope   = Telescope

-- boilerplate -------------------------------------------------------

{-
instance Functor TySig where
  fmap f ts = ts { typePart = f (typePart ts) }
-}

-- eraseMeasure (Delta -> mu -> T) = Delta -> T
eraseMeasure :: Expr -> Expr
eraseMeasure (Quant Pi (TMeasure{}) b) = b -- there can only be one measure!
eraseMeasure (Quant Pi a@(TBind{}) b)  = Quant Pi a $ eraseMeasure b
eraseMeasure (Quant Pi a@(TBound{}) b) = Quant Pi a $ eraseMeasure b
eraseMeasure (LLet a tel e b) = LLet a tel e $ eraseMeasure b
eraseMeasure t = t

-- inferable term = True/False
-- not needed for types or sizes
inferable :: Expr -> Bool
inferable Var{}   = True
inferable Sort{}  = True
inferable Zero{}  = True
inferable Infty{} = True
--inferable Con{}   = True
-- 2012-01-22 constructors are no longer inferable, since parameters are missing
inferable (Def (DefId { idKind = ConK{} }))  = False
inferable Def{} = True
inferable (App f e) = inferable f
-- inferable (Pair f e) = inferable f && inferable e  -- pairs are not inferable due to irrelevant sigma!
-- inferable Sing{}  = True  -- not with universes
inferable _       = False

-- | Collect the variables from the binders
class BoundVars a where
  boundVars :: Collection c Name => a -> c

instance BoundVars a => BoundVars [a] where
  boundVars = foldMap boundVars

instance BoundVars a => BoundVars (Maybe a) where
  boundVars = foldMap boundVars

instance (BoundVars a, BoundVars b) => BoundVars (a, b) where
  boundVars (a, b) = mconcat [boundVars a, boundVars b]

instance (BoundVars a, BoundVars b, BoundVars c) => BoundVars (a, b, c) where
  boundVars (a, b, c) = mconcat [boundVars a, boundVars b, boundVars c]

instance BoundVars (TBinding a) where
  boundVars (TBind x a)  = Coll.singleton x
  boundVars (TMeasure m) = mempty
  boundVars (TBound b)   = mempty

instance BoundVars Telescope where
  boundVars = boundVars . telescope

instance BoundVars (Pat e) where
  boundVars (VarP name)   = Coll.singleton name
  boundVars (SizeP x y)   = Coll.singleton y
  boundVars (SuccP p)     = boundVars p
  boundVars (ConP _ _ ps) = boundVars ps
  boundVars (PairP p p')  = boundVars (p, p')
  boundVars (ProjP _)     = mempty
  boundVars (DotP _)      = mempty
  boundVars (ErasedP p)   = boundVars p
  boundVars (AbsurdP)     = mempty
  boundVars (UnusableP p) = mempty



-- | Boilerplate to extract free variables in the usual sense.
class FreeVars a where
  freeVars :: a -> Set Name

instance FreeVars a => FreeVars [a] where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Maybe a) where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Sort a) where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Dom a) where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Measure a) where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Bound a) where
  freeVars = foldMap freeVars

instance FreeVars a => FreeVars (Tagged a) where
  freeVars = foldMap freeVars

instance (FreeVars a, FreeVars b) => FreeVars (a, b) where
  freeVars (a, b) = mconcat [freeVars a, freeVars b]

instance (FreeVars a, FreeVars b, FreeVars c) => FreeVars (a, b, c) where
  freeVars (a, b, c) = mconcat [freeVars a, freeVars b, freeVars c]

instance FreeVars a => FreeVars (TBinding a) where
  freeVars (TBind x a)  = freeVars a  -- Note: x is bound in the stuff to come, not in a.
  freeVars (TMeasure m) = freeVars m
  freeVars (TBound b)   = freeVars b

instance FreeVars Telescope where
  freeVars (Telescope [])         = mempty
  freeVars (Telescope (tb : tel)) = freeVars tb `Set.union`
                          (freeVars (Telescope tel) Set.\\ boundVars tb)

instance FreeVars Expr where
  freeVars e0 =
    case e0 of
      Sort s    -> freeVars s
      Zero      -> mempty
      Succ e    -> freeVars e
      Infty     -> mempty
      Var name  -> Set.singleton name
      Def{}     -> mempty
      Case e mt cls
                -> freeVars (e, mt, cls)
      LLet (TBind x dom) tel t u | null tel
                -> freeVars (dom, t) `Set.union` Set.delete x (freeVars u)
      Pair f e  -> freeVars (f, e)
      App  f e  -> freeVars (f, e)
      Max  es   -> freeVars es
      Plus es   -> freeVars es
      Lam _ x e -> Set.delete x (freeVars e)
      Quant pisig ta b -> freeVars ta `Set.union` (freeVars b Set.\\ boundVars ta)
{-
      Quant pisig tel ta b
                -> freeVars tel' `Set.union` (freeVars b Set.\\ boundVars tel')
                     where tel' = Telescope $ telescope tel ++ [ta]
-}
      Sing e t  -> freeVars (e, t)
      Below _ e -> freeVars e
      Ann te    -> freeVars te
      Irr       -> mempty
      e         -> error $ "freeVars " ++ show e ++ " not implemented"

instance FreeVars Clause where
  freeVars (Clause _ ps Nothing)  = mempty  -- absurd clause
  freeVars (Clause _ ps (Just e)) = freeVars e Set.\\ boundVars ps

patternVars :: Pattern -> [Name]
patternVars = boundVars
{-
patternVars (VarP name)   = [name]
patternVars (SizeP x y)   = [y]
patternVars (SuccP p)     = patternVars p
patternVars (ConP _ _ ps) = List.concat $ List.map patternVars ps
patternVars (PairP p p')  = patternVars p ++ patternVars p'
patternVars (DotP _)      = []
patternVars (ErasedP p)   = patternVars p
patternVars (AbsurdP)     = []
-}

-- | Get all the definitions that are refered to in expression.
--   This is used e.g. to check whether a (co)fun is recursive.
class UsedDefs a where
  usedDefs :: a -> [Name]

instance UsedDefs a => UsedDefs [a] where
  usedDefs = foldMap usedDefs

instance UsedDefs a => UsedDefs (Maybe a) where
  usedDefs = foldMap usedDefs

instance UsedDefs a => UsedDefs (Sort a) where
  usedDefs = foldMap usedDefs

instance UsedDefs a => UsedDefs (Dom a) where
  usedDefs = foldMap usedDefs

instance UsedDefs a => UsedDefs (Measure a) where
  usedDefs = foldMap usedDefs

instance UsedDefs a => UsedDefs (Bound a) where
  usedDefs = foldMap usedDefs

instance UsedDefs a => UsedDefs (Tagged a) where
  usedDefs = foldMap usedDefs

instance (UsedDefs a, UsedDefs b) => UsedDefs (a, b) where
  usedDefs (a, b) = mconcat [usedDefs a, usedDefs b]

instance (UsedDefs a, UsedDefs b, UsedDefs c) => UsedDefs (a, b, c) where
  usedDefs (a, b, c) = mconcat [usedDefs a, usedDefs b, usedDefs c]

instance (UsedDefs a, UsedDefs b, UsedDefs c, UsedDefs d) => UsedDefs (a, b, c, d) where
  usedDefs (a, b, c, d) = mconcat [usedDefs a, usedDefs b, usedDefs c, usedDefs d]

instance UsedDefs a => UsedDefs (TBinding a) where
  usedDefs (TBind _ e)  = usedDefs e
  usedDefs (TMeasure m) = usedDefs m
  usedDefs (TBound b)   = usedDefs b

instance UsedDefs Telescope where
  usedDefs = usedDefs . telescope

instance UsedDefs DefId where
  usedDefs id
    | idKind id `elem` [FunK, DatK] = [unqual $ idName id]
    | otherwise                     = []

instance UsedDefs Clause where
  usedDefs = usedDefs . clExpr

instance UsedDefs Expr where
  usedDefs (Def id)           = usedDefs id
  usedDefs (Pair f e)         = usedDefs (f, e)
  usedDefs (App f e)          = usedDefs (f, e)
  usedDefs (Max es)           = usedDefs es
  usedDefs (Plus es)          = usedDefs es
  usedDefs (Lam _ x e)        = usedDefs e
  usedDefs (Sing a b)         = usedDefs (a, b)
  usedDefs (Below _ b)        = usedDefs b
--  usedDefs (Quant _ tel tb b) = usedDefs (tel, tb, b)
  usedDefs (Quant _ tb b)     = usedDefs (tb, b)
  usedDefs (LLet tb tel e1 e2)= usedDefs (tb, tel, e1, e2)
  usedDefs (Succ e)           = usedDefs e
  usedDefs (Case e mt cls)    = usedDefs (e, mt, cls)
  usedDefs (Ann e)            = usedDefs e
  usedDefs (Sort s)           = usedDefs s
  usedDefs Zero               = []
  usedDefs Infty              = []
  usedDefs Meta{}             = []
  usedDefs Var{}              = []
  usedDefs Proj{}             = []
  usedDefs (Record ri rs)     = foldMap (usedDefs . snd) rs
  usedDefs e                  = error $ "usedDefs " ++ show e ++ " not implemented"

rhsDefs :: [Clause] -> [Name]
rhsDefs cls = List.foldl (\ ns (Clause _ ps e) -> maybe [] usedDefs e ++ ns) [] cls

-- pretty printing expressions ---------------------------------------

[precArrL, precAppL, precAppR] = [1..3]

instance Pretty Name where
--  pretty x = text $ suggestion x
  pretty x = text $ show x

instance Pretty QName where
  pretty (Qual m n) = pretty m <> text "." <> pretty n
  pretty (QName n)  = pretty n

instance Pretty DefId where
--    pretty d = pretty $ name d
    pretty d = text $ show d

instance Pretty Expr where
  prettyPrec _ Irr         = text "."
  prettyPrec k (Sort s)    = prettyPrec k s
  prettyPrec _ Zero        = text "0"
  prettyPrec _ Infty       = text "#"
  prettyPrec _ (Meta i)    = text $ "?" ++ show i
  prettyPrec _ (Var n)     = pretty n
--  prettyPrec _ (Con _ n)   = text n
  prettyPrec _ (Def id)    = pretty id
--  prettyPrec _ (Let n)     = text n
  prettyPrec _ (Sing e t)  = angleBrackets $ pretty e <+> colon <+> pretty t
  prettyPrec k e@Succ{}    =
    case succView e of
      (n, Zero) -> text $ show n
      (n, e)    -> text (replicate n '$') <> prettyPrec precAppR e
--  prettyPrec k (Succ e)    = text "$" <> prettyPrec precAppR e
{-  prettyPrec k (Succ e)    = parensIf (precAppR <= k) $
                              text "$" <+> prettyPrec precAppR e   -}
  prettyPrec k (Max es)  = parensIf (precAppR <= k) $
    List.foldl (\ d e -> d <+> prettyPrec precAppR e) (text "max") es
  prettyPrec k (Plus (e:es))  = parensIf (1 < k) $
    List.foldl (\ d e -> d <+> text "+" <+> prettyPrec 1 e) (prettyPrec 1 e) es
  prettyPrec k (Proj Pre n)   = pretty n
  prettyPrec k (Proj Post n)  = text "." <> pretty n
  prettyPrec k (Record AnonRec []) = text "record" <+> braces empty
  prettyPrec k (Record AnonRec rs) = text "record" <+> prettyRecFields rs
  prettyPrec k (Record (NamedRec _ n _ dotted) []) = dotIf dotted $ pretty n
  prettyPrec k (Record (NamedRec _ n True dotted) rs) = dotIf dotted $ pretty n <+> prettyRecFields rs
  prettyPrec k (Record (NamedRec _ n False dotted) rs) =
   parensIf (not (null rs) && precAppR <= k) $ dotIf dotted $
     pretty n <+> hsep (List.map (prettyPrec precAppR . snd) rs)
  prettyPrec k (Pair e1 e2) = parens $ pretty e1 <+> comma <+> pretty e2
  prettyPrec k (App f e)  = parensIf (precAppR <= k) $
    prettyPrec precAppL f <+> prettyPrec precAppR e
--   prettyPrec k (App e [])  = prettyPrec k e
--   prettyPrec k (App e es)  = parensIf (precAppR <= k) $
--     List.foldl (\ d e -> d <+> prettyPrec precAppR e) (prettyPrec precAppL e) es
  prettyPrec k (Case e mt cs) = parensIf (0 < k) $
    (text "case" <+> pretty e) <+> (maybe empty (\ t -> colon <+> pretty t) mt) $$ (vlist $ List.map prettyCase cs)
  prettyPrec k (Lam dec x e) = parensIf (0 < k) $
    (if erased dec then brackets else id) (text "\\" <+> pretty x <+> text "->")
      <+> pretty e
  prettyPrec k (LLet (TBind n (Domain mt ki dec)) tel e1 e2) | null tel = parensIf (0 < k) $
    (text "let" <+> ((if erased dec then lbrack else PP.empty) <>
       pretty n <+> vcat [ maybe empty (\ t -> colon <+> pretty t) mt
                           <> (if erased dec then rbrack else PP.empty)
                       , equals <+> pretty e1 ]))
    $$ (text "in" <+> pretty e2)
  prettyPrec k (LLet (TBind n (Domain mt ki dec)) tel e1 e2) = parensIf (0 < k) $
    (text "let" <+> ((if erased dec then brackets else id) $ pretty n)
                <+> pretty tel
                <+> vcat [ maybe empty (\ t -> colon <+> pretty t) mt
                         , equals <+> pretty e1 ])
    $$ (text "in" <+> pretty e2)
{-
  prettyPrec k (LLet (TBind n (Domain Nothing ki dec)) e1 e2) = parensIf (0 < k) $
    (text "let" <+> ((if erased dec then lbrack else PP.empty) <>
       pretty n <+> vcat [ if erased dec then rbrack else PP.empty
                         , equals <+> pretty e1 ]))
    $$ (text "in" <+> pretty e2)
-}
  prettyPrec k (Below ltle e) = pretty ltle <+> prettyPrec k e
  prettyPrec k (Quant Pi (TMeasure mu) t2) = parensIf (precArrL <= k) $
    (pretty mu <+> text "->" <+> pretty t2)
  prettyPrec k (Quant Pi (TBound beta) t2) = parensIf (precArrL <= k) $
    (pretty beta <+> text "->" <+> pretty t2)

  prettyPrec k (Quant pisig (TBind x (Domain t1 ki dec)) t2) | null (suggestion x) = parensIf (precArrL <= k) $
    ((if erased dec then ppol <> brackets (pretty t1)
       else ppol <+> prettyPrec precArrL t1)
      <+> pretty pisig <+> pretty t2)
    where pol = polarity dec
          ppol = if pol==defaultPol then PP.empty else text $ show pol

  prettyPrec k (Quant pisig (TBind x (Domain (Below ltle t1) ki dec)) t2) = parensIf (precArrL <= k) $
    ppol <>
    ((if erased dec then brackets else parens) $
      pretty x <+> pretty ltle <+> pretty t1) <+> pretty pisig <+> pretty t2
    where pol = polarity dec
          ppol = if pol==defaultPol then PP.empty else text $ show pol

  prettyPrec k (Quant pisig (TBind x (Domain t1 ki dec)) t2) = parensIf (precArrL <= k) $
    ppol <>
    ((if erased dec then brackets else parens) $
      pretty x <+> colon <+> pretty t1) <+> pretty pisig <+> pretty t2
    where pol = polarity dec
          ppol = if pol==defaultPol then PP.empty else text $ show pol

  prettyPrec k (Ann e) = pretty e

class DotIf a where
  dotIf :: a -> Doc -> Doc

instance DotIf Bool where
  dotIf False d = d
  dotIf True  d = text "." <> d

instance DotIf Dotted where
  dotIf c = dotIf (isDotted c)

instance Pretty TBind where
  prettyPrec k (TMeasure mu) = pretty mu
  prettyPrec k (TBound beta) = pretty beta

  prettyPrec k (TBind x (Domain (Below ltle t1) ki dec)) =
    ppol <>
    ((if erased dec then brackets else parens) $
      pretty x <+> pretty ltle <+> pretty t1)
    where pol = polarity dec
          ppol = if pol==defaultPol then PP.empty else text $ show pol

  prettyPrec k (TBind x (Domain t1 ki dec)) =
    ppol <>
    ((if erased dec then brackets else parens) $
      pretty x <+> colon <+> pretty t1)
    where pol = polarity dec
          ppol = if pol==defaultPol then PP.empty else text $ show pol

instance Pretty Telescope where
  prettyPrec k tel = sep $ map pretty $ telescope tel

prettyRecFields rs =
    let l:ls = List.map (\ (n, e) -> pretty n <+> equals <+> prettyPrec 0 e) rs
    in  cat $ (lbrace <+> l) : List.map (semi <+>) ls ++ [empty <+> rbrace]

prettyCase (Clause _ [p] Nothing)  = pretty p
prettyCase (Clause _ [p] (Just e)) = pretty p <+> text "->" <+> pretty e

instance Pretty PiSigma where
  pretty Pi    = text "->"
  pretty Sigma = text "&"

vlist :: [Doc] -> Doc
vlist [] = lbrace <> rbrace
vlist ds = (vcat $ zipWith (<+>) (lbrace : repeat semi) ds) $$ rbrace

instance Pretty (Measure Expr) where
  pretty (Measure es) = text "|" <> hsepBy comma (List.map pretty es) <> text "|"

instance Pretty LtLe where
  pretty Lt = text "<"
  pretty Le = text "<="

instance Pretty (Bound Expr) where
  pretty (Bound ltle mu mu') = pretty mu <+> pretty ltle <+> pretty mu'

{-
instance Pretty (Bound Expr) where
  pretty (Bound mu mu') = case predecessor mu' of
    Nothing -> pretty mu <+> text "<" <+> pretty mu'
    Just mu' -> pretty mu <+> text "<=" <+> pretty mu'
-}


instance Pretty (Sort Expr) where
  prettyPrec k (SortC c)  = text $ show c
  prettyPrec k (Set Zero) = text "Set" -- print as Set for backwards compat.
  prettyPrec k (Set e) =  parensIf (precAppR <= k) $
    text "Set" <+> prettyPrec precAppR e
  prettyPrec k (CoSet e) = parensIf (precAppR <= k) $
    text "CoSet" <+> prettyPrec precAppR e

instance Pretty Pattern where
  prettyPrec k (VarP x)       = pretty x
  prettyPrec k (ConP co c ps) = parensIf (not (null ps) && precAppR <= k) $
    -- (if dottedPat co then text "." else empty) <>
    dotIf (dottedPat co) $ pretty c <+> hsep (List.map (prettyPrec precAppR) ps)
  prettyPrec k (SuccP p)      = text "$" <> prettyPrec k p
  prettyPrec k (SizeP x y)    = parensIf (precAppR <= k) $ pretty y <+> text "<" <+> pretty x
  prettyPrec k (PairP p p')   = parens $ pretty p <> comma <+> pretty p'
  prettyPrec k (UnusableP p)  = prettyPrec k p
  prettyPrec k (ProjP x)      = text "." <> pretty x
  prettyPrec k (DotP p)       = text "." <> prettyPrec precAppR p
  prettyPrec k (AbsurdP)      = text "()"
  prettyPrec k (ErasedP p)    = brackets $ prettyPrec 0 p


instance Show Expr where
  showsPrec k e s = render (prettyPrec k e) ++ s
  -- show = render . pretty -- showExpr

instance Show Pattern where
  show = render . pretty

showCase (Clause _ [p] Nothing) = render (prettyPrec precAppR p)
showCase (Clause _ [p] (Just e)) = render (prettyPrec precAppR p) ++ " -> " ++ show e
showCases = showList "; " showCase



-- substitution ------------------------------------------------------

{-
class PatSubst p where
  patSubst :: [(Name, Expr)] -> p -> p

instance PatSubst Name where
  patSubst phi n = maybe p id $ lookup n phi
-}

-- | substitute into pattern
patSubst :: [(Name, Pattern)] -> Pattern -> Pattern
patSubst phi p =
  let phi' x = maybe (Var x) patternToExpr $ lookup x phi
  in
  case p of
    VarP n -> maybe p id $ lookup n phi
    ConP pi n ps -> ConP pi n $ List.map (patSubst phi) ps
    SuccP p      -> SuccP $ patSubst phi p
    SizeP e y    -> SizeP (parSubst phi' e) y
    PairP p1 p2  -> PairP (patSubst phi p1) (patSubst phi p2)
    ProjP x      -> p
    DotP e       -> DotP $ parSubst phi' e
    AbsurdP      -> p
    ErasedP p    -> ErasedP $ patSubst phi p
    UnusableP p   -> UnusableP $ patSubst phi p

-- parallel substitution (CAUTION! NOT CAPTURE AVOIDING!)
-- only needed to generate destructors
-- does not substitute into patterns of a Case

class ParSubst a where
  parSubst :: (Name -> Expr) -> a -> a

instance ParSubst a => ParSubst [a] where
  parSubst = map . parSubst

instance ParSubst a => ParSubst (Maybe a) where
  parSubst = fmap . parSubst

instance ParSubst a => ParSubst (Dom a) where
  parSubst = fmap . parSubst

instance ParSubst a => ParSubst (Measure a) where
  parSubst = fmap . parSubst

instance ParSubst a => ParSubst (Bound a) where
  parSubst = fmap . parSubst

instance ParSubst a => ParSubst (Tagged a) where
  parSubst = fmap . parSubst

instance ParSubst a => ParSubst (TBinding a) where
  parSubst phi (TBind x a)  = TBind x  $ parSubst phi a
  parSubst phi (TMeasure m) = TMeasure $ parSubst phi m
  parSubst phi (TBound b)   = TBound   $ parSubst phi b

instance ParSubst a => ParSubst (Sort a) where
  parSubst phi (CoSet e) = CoSet $ parSubst phi e
  parSubst phi (Set e)   = Set   $ parSubst phi e
  parSubst phi s         = s

instance ParSubst Telescope where
  parSubst phi = Telescope . parSubst phi . telescope

instance ParSubst Clause where
  parSubst phi (Clause tel ps e) = Clause tel ps $ parSubst phi e

-- TODO: Refactor!
instance ParSubst Expr where
  parSubst phi (Sort s)              =  Sort $ parSubst phi s
  parSubst phi (Succ e)              = Succ (parSubst phi e)
  parSubst phi e@Zero                = e
  parSubst phi e@Infty               = e
  parSubst phi e@Meta{}              = e
  parSubst phi e@Proj{}              = e
  parSubst phi (Var x)               = phi x
  parSubst phi e@Def{}               = e
  parSubst phi (Case e mt cls)       = Case (parSubst phi e) (parSubst phi mt) (parSubst phi cls)
  parSubst phi (LLet ta tel b c)     = LLet (parSubst phi ta) (parSubst phi tel) (parSubst phi b) (parSubst phi c)
  parSubst phi (Pair f e)            = Pair (parSubst phi f) (parSubst phi e)
  parSubst phi (App f e)             = App (parSubst phi f) (parSubst phi e)
  parSubst phi (Record ri rs)        = Record ri (mapAssoc (parSubst phi) rs)
  parSubst phi (Max es)              = Max (parSubst phi es)
  parSubst phi (Plus es)             = Plus (parSubst phi es)
  parSubst phi (Lam dec x e)         = Lam dec x (parSubst phi e)
  parSubst phi (Below ltle e)        = Below ltle (parSubst phi e)
  parSubst phi (Quant pisig a b)     = Quant pisig (parSubst phi a) (parSubst phi b)
--  parSubst phi (Quant pisig tel a b) = Quant pisig (parSubst phi tel) (parSubst phi a) (parSubst phi b)
  parSubst phi (Sing a b)            = Sing (parSubst phi a) (parSubst phi b)
  parSubst phi (Ann e)               = Ann $ parSubst phi e
  parSubst phi e                     = error $ "Abstract.parSubst phi (" ++ show e ++ ") undefined"
  {- NOT NEEDED
  sgSubst :: Name -> Expr -> Expr -> Expr
  sgSubst x t u = parSubst (\ y -> if x == y then t else Var y) u
  -}


-- | Metavariable substitution. (BY INTENTION NOT CAPTURE AVOIDING!)
--   Does not substitute in patterns!
class Substitute a where
  subst :: Subst -> a -> a

instance Substitute a => Substitute [a] where
  subst = map . subst

instance Substitute a => Substitute (Maybe a) where
  subst = fmap . subst

instance Substitute a => Substitute (Dom a) where
  subst = fmap . subst

instance Substitute a => Substitute (Measure a) where
  subst = fmap . subst

instance Substitute a => Substitute (Bound a) where
  subst = fmap . subst

instance Substitute a => Substitute (Tagged a) where
  subst = fmap . subst

instance Substitute a => Substitute (TBinding a) where
  subst phi (TBind x a)  = TBind x  $ subst phi a
  subst phi (TMeasure m) = TMeasure $ subst phi m
  subst phi (TBound b)   = TBound   $ subst phi b

instance Substitute a => Substitute (Sort a) where
  subst phi (CoSet e) = CoSet $ subst phi e
  subst phi (Set e)   = Set   $ subst phi e
  subst phi s         = s

instance Substitute Telescope where
  subst phi = Telescope . subst phi . telescope

instance Substitute Clause where
  subst phi (Clause tel ps e) = Clause tel ps $ subst phi e

instance Substitute Expr where
  subst phi (Sort s)              = Sort $ subst phi s
  subst phi (Succ e)              = Succ (subst phi e)
  subst phi e@Zero                = e
  subst phi e@Infty               = e
  subst phi e@(Meta i)            = Map.findWithDefault e i phi
  subst phi e@Var{}               = e
  subst phi e@Def{}               = e
  subst phi e@Proj{}              = e
  subst phi (Case e mt cls)       = Case (subst phi e) (subst phi mt) (subst phi cls)
  subst phi (LLet ta tel b c)     = LLet (subst phi ta) (subst phi tel) (subst phi b) (subst phi c)
  subst phi (Pair f e)            = Pair (subst phi f) (subst phi e)
  subst phi (App f e)             = App (subst phi f) (subst phi e)
  subst phi (Record ri rs)        = Record ri (mapAssoc (subst phi) rs)
  subst phi (Max es)              = Max (subst phi es)
  subst phi (Plus es)             = Plus (subst phi es)
  subst phi (Lam dec x e)         = Lam dec x (subst phi e)
  subst phi (Below ltle e)        = Below ltle (subst phi e)
  subst phi (Quant pisig a b)     = Quant pisig (subst phi a) (subst phi b)
--  subst phi (Quant pisig tel a b) = Quant pisig (subst phi tel) (subst phi a) (subst phi b)
  subst phi (Sing a b)            = Sing (subst phi a) (subst phi b)
  subst phi (Ann e)               = Ann $ subst phi e
  subst phi e                     = error $ "Abstract.subst phi (" ++ show e ++ ") undefined"

-- Printing declarations ---------------------------------------------

{-
instance Show Declaration where
  show = render . pretty

instance Pretty Declaration
  pretty (DataD
-}

-- pretty print a function body
prettyFun :: Name -> [Clause] -> Doc
prettyFun f cls = vlist $ List.map (prettyClause f) cls

prettyClause f (Clause _ ps Nothing) = pretty f <+> hsep (List.map (prettyPrec precAppR) ps)
prettyClause f (Clause _ ps (Just e)) = pretty f
  <+> hsep (List.map (prettyPrec precAppR) ps)
  <+> equals <+> pretty e

-- Constructor analysis ----------------------------------------------

data FieldClass
  = Index                    -- ^ E.g., the length in Vector.
  | NotErasableIndex         -- ^ E.g., @c : (index : A) -> D (f index)@
  | Field (Maybe Destructor) -- ^ An actual field, not free in the target.
    deriving (Eq, Show)

type Destructor = (Type, Arity, Clause)

data FieldInfo = FieldInfo
  { fDec   :: Dec
  , fName  :: Name        -- ^ Empty "" for anonymous fields.
  , fType  :: Type        -- ^ Naked type (no preceeding telescope).
--  , fLazy  :: Bool        -- lazy (coinductive occ) or strict (everything else) -- see TCM.hs ConSig
  , fClass :: FieldClass
  }

instance Show FieldInfo where
  show (FieldInfo dec name t fcl) =
    (if fcl == Index then "index " else "field ") ++
    bracketsIf (erased dec) (show name ++ " : " -- ++ (if lazy then "?" else "")
                                      ++ show t)

data PatternsType
  = NotPatterns        -- at least "pattern" is none
  | LinearPatterns     -- the patterns do not share a common var
  | NonLinearPatterns  -- the patterns share a common var
    deriving (Eq, Ord, Show)

data ConstructorInfo = ConstructorInfo
  { cName   :: QName
--  , cType   :: TVal
  , cPars   :: ParamPats  -- ^ Constructor parameters if unequal to data parameters.
  , cFields :: [FieldInfo]
  , cTyCore :: Type
  , cPatFam :: (PatternsType, [Pattern])
  , cEtaExp :: Bool -- all destructors are defined, family pattern is non-overlapping with family patterns of other constructors
  , cRec    :: Bool -- constructor has recursive fields
  } deriving Show

corePat :: ConstructorInfo -> [Pattern]
corePat = snd . cPatFam

{- Old comment:
a record type is a data type that fulfills 3 conditions
   1. non-recursive
   2. exactly 1 constructor
   3. constructor carries names for each of its arguments

Non-indexed case: generate destructors

  data Sigma (A : Set) (B : A -> Set) : Set
  { pair : (fst : A) -> (snd : B fst) -> Sigma A B
  }
  fst : [A : Set] -> [B : A -> Set] -> (p : Sigma A B) -> A
  { fst A B (pair _fst _snd) = _fst }
  snd : [A : Set] -> [B : A -> Set] -> (p : Sigma A B) -> B (fst p)
  { snd A B (pair _fst _snd) = _snd }

-}
{- Indexed case: For the constructor

  vcons : (n : Nat) -> (head : A) -> (tail : Vec A n) -> Vec A (suc n)

cName   = "vcons"
-- cType   = evaluation of (A : Set) -> (n : Nat) -> ...
cFields = [("n",Nat,Index),("head",A,Field),("tail",Vec A n,Field)]
cTyCore = Vec A (suc n)
cPatFam = (True, [A, suc n])
cEtaExp = True, but may be set to False later since the constructor is recursive

We generate the destructors

  head : (A : Set) -> (n : Nat) -> (x : Vec A (suc n)) -> A
  head A n (vcons .n _head _tail) = _head

  tail : (A : Set) -> (n : Nat) -> (x : Vec A (suc n)) -> Vec A n
  tail A n (vcons .n _head _tail) = _tail

in the implementation we use "constructed_by_head" for "x"

discriminate index arguments from fields
  - split constructor type into telescope and core
    [(n : Nat),(head : A),(tail : Vec A n)], Vec A (suc n)
  - find free variables of core: [A,n]
  - create a list of (name,type,classification) for each constructor arg,
    where classification in {index,field}

-}

-- TODO: analyze value, not expression!
-- get all the variables which are under injective functions

class InjectiveVars a where
  injectiveVars :: a -> Set Name

instance InjectiveVars a => InjectiveVars [a] where
  injectiveVars = foldMap injectiveVars

instance InjectiveVars a => InjectiveVars (Maybe a) where
  injectiveVars = foldMap injectiveVars

instance InjectiveVars a => InjectiveVars (Sort a) where
  injectiveVars = foldMap injectiveVars

instance InjectiveVars a => InjectiveVars (Dom a) where
  injectiveVars = foldMap injectiveVars

instance InjectiveVars a => InjectiveVars (Measure a) where
  injectiveVars = foldMap injectiveVars

instance InjectiveVars a => InjectiveVars (Bound a) where
  injectiveVars = foldMap injectiveVars

instance InjectiveVars a => InjectiveVars (Tagged a) where
  injectiveVars = foldMap injectiveVars

instance (InjectiveVars a, InjectiveVars b) => InjectiveVars (a, b) where
  injectiveVars (a, b) = mconcat [injectiveVars a, injectiveVars b]

instance (InjectiveVars a, InjectiveVars b, InjectiveVars c) => InjectiveVars (a, b, c) where
  injectiveVars (a, b, c) = mconcat [injectiveVars a, injectiveVars b, injectiveVars c]

instance InjectiveVars a => InjectiveVars (TBinding a) where
  injectiveVars (TBind x a)  = injectiveVars a
  injectiveVars (TMeasure m) = injectiveVars m
  injectiveVars (TBound b)   = injectiveVars b

instance InjectiveVars Telescope where
  injectiveVars (Telescope []) = mempty
  injectiveVars (Telescope (tb : tel)) = injectiveVars tb `Set.union`
                          (injectiveVars (Telescope tel) Set.\\ boundVars tb)

instance InjectiveVars Expr where
  injectiveVars e =
   case spineView e of
    (Var name            , []) -> Set.singleton name
    (Def (DefId DatK{} _), es) -> injectiveVars es
    (Def (DefId ConK{} _), es) -> injectiveVars es
    (Record ri rs        , []) -> Set.unions $ List.map (injectiveVars . snd) rs
    (Succ e              , []) -> injectiveVars e
    (Lam _ x e           , []) -> Set.delete x (injectiveVars e)
    (Quant _ ta b , []) -> injectiveVars ta `Set.union` (injectiveVars b Set.\\ boundVars ta)
--     (Quant _ tel ta b , []) ->
--       injectiveVars tel' `Set.union` (injectiveVars b Set.\\ boundVars tel')
--         where tel' = Telescope $ telescope tel ++ [ta]
--     (Sort s             , []) -> injectiveVars s
    (Ann e              , []) -> injectiveVars e
    _                         -> Set.empty

classifyFields :: Co -> Name -> Type -> [FieldInfo]
classifyFields co dataName ty = List.map (classifyField fvs) $ telescope tele
  where (tele, core) = typeToTele ty
        fvs = freeVars core
        ivs = injectiveVars core
        classifyField fvs (TBind name (Domain ty ki dec)) = FieldInfo
          { fDec = dec
          , fName  = name
          , fType  = ty
--          , fLazy  = co == CoInd && maybeRecursiveOccurrence dataName ty
          , fClass = if name `Set.member` fvs then
                       if name `Set.member` ivs then Index else NotErasableIndex
                      else Field Nothing
          }

isField :: FieldClass -> Bool
isField Field{} = True
isField _       = False

isNamedField :: FieldInfo -> Bool
isNamedField f = isField (fClass f) && not (erased $ fDec f) && not (emptyName $ fName f)

destructorNames :: [FieldInfo] -> [Name]
destructorNames fields = List.map fName $ filter isNamedField fields

analyzeConstructor :: Co -> Name -> Telescope -> Constructor -> ConstructorInfo
analyzeConstructor co dataName dataPars (Constructor constrName conPars ty) =
  let (_, core)  = typeToTele ty
      pars       = maybe dataPars fst conPars
      fields     = classifyFields co dataName ty
      -- freshenFieldName fi = fi { fName = freshen $ fName fi }
      -- freshfields = List.map freshenFieldName fields
      -- generate destructors
      -- choose a name for the record to destroy
      indices    = filter (\ f -> fClass f == Index) fields
      indexTele  = Telescope $ List.map (\ f -> TBind (fName f) $ Domain (fType f) defaultKind (fDec f)) indices
      indexNames  = List.map fName indices
      -- do not generated destructors for erased arguments
      destrNames = destructorNames fields
      recName    = internal $ name constrName -- "constructed_by_" ++ constrName
      parNames   = List.map boundName $ telescope pars
      parAndIndexNames = parNames ++ indexNames
      -- substitute variable "fst" by application "fst A B p"
      phi x = if x `elem` destrNames
                then List.foldl App ({-fun x-} letdef x) (List.map Var (parAndIndexNames ++ [recName]))
                else Var x
      -- prefix d =  "destructor_argument_" ++ d
      prefix d = d { suggestion = "#" ++ suggestion d }
      -- modifiedDestrNames = List.map prefix destrNames
      -- TODO: Index arguments are not always before fields
      pattern = ConP (PatternInfo (coToConK co) False False) -- to bootstrap destructor, not irrefutable
          constrName
          ( -- 2012-01-22 PARS GONE!   List.map (DotP . Var) parNames ++
            List.map (\ fi -> (case fClass fi of
                            Index   -> DotP . Var
                            Field{} -> VarP . prefix)
                         (fName fi))
              fields)
      destrType t = -- teleToTypeErase (pars ++ indexTele)
                    teleToTypeErase pars $ teleToType indexTele $
                      pi (TBind recName $ defaultDomain core) $ parSubst phi t
      destrBody (dn) = clause (List.map VarP parAndIndexNames ++ [pattern]) (Just (Var dn))
      fields' = mapOver fields $
        \ f -> if isNamedField f then
                  f { fClass = Field $ Just
                         ( destrType (fType f)
                         , let npars = size pars
                           in  Arity { fullArity = npars + size indexTele + 1
                                     , isProjection = Just npars
                                     }
                         , destrBody (prefix (fName f)) )}
                else f
      computeLinearity :: (Bool, [Pattern]) -> (PatternsType, [Pattern])
      computeLinearity (False, ps) = (NotPatterns, ps)
      computeLinearity (True , ps) = (if linear then LinearPatterns else NonLinearPatterns, ps) where
        linear = List.null ps || (List.null $ List.foldl1 List.intersect $ List.map patternVars ps)

      result = ConstructorInfo
       { cName   = constrName
       , cPars   = conPars
       , cFields = fields'
       , cTyCore = core
       -- check whether core is D ps and store pats; also compute whether ps are linear
       , cPatFam = computeLinearity $ fromAllWriter $ isPatIndFamC core
       , cEtaExp = destructorNamesPresent fields
       , cRec    = True  -- we don't know here, assume the worst
       }
   in -- trace ("analyzeConstructor returns " ++ show result) $
        result

-- can only eta expand if I can generate all destructors
destructorNamesPresent :: [FieldInfo] -> Bool
destructorNamesPresent fields =
  all (\ f -> fClass f /= NotErasableIndex &&  -- no bad index
              (fClass f == Index ||
               not (erased $ fDec f) && not (emptyName $ fName f))) -- no erased or unnamed field
    fields

-- | Analyze all constructors of a data type at once
--   so that we can also check which constructors patterns are irrefutable.
analyzeConstructors :: Co -> Name -> Telescope -> [Constructor] -> [ConstructorInfo]
analyzeConstructors co dataName pars cs =
  let cis = List.map (analyzeConstructor co dataName pars) cs
      -- check if patterns overlaps with any other
      overlapList = zipWith (\ ci n -> any (overlaps (corePat ci)) $ List.map corePat $ take n cis ++ drop (n+1) cis) cis [0..] -- worst case quadratic, could be improved by exploiting symmetry
      result = zipWith (\ ci ov -> if ov then ci { cEtaExp = False } else ci) cis overlapList
  in result

-- | Build constructor type from constructor info, erasing all indices.
reassembleConstructor :: ConstructorInfo -> Constructor
reassembleConstructor ci = Constructor (cName ci) (cPars ci) (reassembleConstructorType ci)

-- | Assumes that all the indices (even from data telescope) are contained
--   in fields.
reassembleConstructorType :: ConstructorInfo -> Type
reassembleConstructorType ci = buildPi (cFields ci) where
  buildPi [] = cTyCore ci
  buildPi (f:fs) = pi (TBind (fName f) $ Domain (fType f) defaultKind (decor (fDec f) (fClass f))) $ buildPi fs
    where decor dec Index = irrelevantDec -- DONE: SWITCH ON!
          decor dec _     = dec

-- Pattern inductive families ----------------------------------------

-- isPatIndFam takes a list of type signatures (constructor decls.)
-- and checks whether we have a pattern inductive family
-- in this case, a list of constructors with the associated
-- type indices (translated into pattern list) is returned
-- type parameters are dropped
{-
isPatIndFam :: Int -> [Constructor] -> Maybe [(Name,[Pattern])]
isPatIndFam numPars= mapM (\ tysig ->
                             fmap (\ ps -> (namePart tysig, drop numPars ps))
                                  (isPatIndFamC (typePart tysig)))
-}

-- isPatIndFamC checks whether an expression (the type of s constructor)
-- is of the form
--   Gamma -> D ps
-- and returns the list ps of patterns if it is the case
isPatIndFamC :: Expr -> Writer All [Pattern]
isPatIndFamC (Def id) = return []
isPatIndFamC (App f e) = do
  ps <- isPatIndFamC f
  p  <- exprToDotPat' e
  return $ ps ++ [p]
-- isPatIndFamC (App e es) = do
--   ps  <- isPatIndFamC e
--   ps' <- mapM exprToDotPat' es
--   return $ ps ++ ps'
isPatIndFamC (Quant Pi _ e) = isPatIndFamC e
isPatIndFamC _ = tell (All False) >> return []

-- Pattern auxiliary functions ---------------------------------------

-- extract all subpatterns of the form y > x and arrange them in a
-- TreeShapedOrder
tsoFromPatterns :: [Pattern] -> TSO Name
tsoFromPatterns ps = TSO.fromList $ List.concat $ List.map loop ps where
  loop (SizeP (Var father) son) = [(son,(1,father))]
  loop (SizeP (Succ (Var father)) son) = [(son,(0,father))]
  loop (SizeP e      son) = []
  loop (ConP _ _ ps)      = List.concat $ List.map loop ps
  loop (PairP p p')       = loop p ++ loop p'
  loop (SuccP   p)        = loop p
  loop (ErasedP p)        = loop p
  loop ProjP{}            = []
  loop VarP{}             = []
  loop DotP{}             = []
  loop UnusableP{}        = []

-- for non-dot patterns, patterns overlap if one matches against the other
-- infinity size is represented as (DotP Infty)
-- I reprogram it here, since it does not need a monad
overlap :: Pattern -> Pattern -> Bool
overlap (VarP _) p' = True
overlap p (VarP _)  = True
overlap (ConP _ c ps) (ConP _ c' ps') = c == c' && overlaps ps ps' -- only source of non-overlap
overlap (PairP p1 p2) (PairP p1' p2') = overlaps [p1,p2] [p1',p2']
overlap (ProjP n) (ProjP n') = n == n' -- another source of non-overlap
-- size patterns always overlap
overlap (SuccP p) _ = True
overlap _ (SuccP p) = True
overlap SizeP{} _   = True
overlap _ SizeP{}   = True
-- dot patterns always overlap (safe approximation)
overlap (DotP _) _ = True
overlap _ (DotP _) = True
{-
overlap (SuccP p) (SuccP p') = overlap p p'
overlap (SuccP p) (DotP Infty) = overlap p (DotP Infty)
overlap (DotP Infty) (SuccP p') = overlap (DotP Infty) p'
overlap (DotP Infty) (DotP Infty) = True
-}

overlaps :: [Pattern] -> [Pattern] -> Bool
overlaps ps ps' = and $ zipWith overlap ps ps'

-- | @exprToPattern@ is used in the termination checker to convert
--   dot patterns into proper patterns.
exprToPattern :: Expr -> Maybe Pattern
exprToPattern (Def (DefId (ConK co) n)) = return $ ConP pi n []
  where pi = PatternInfo co False False -- not irrefutable (TODO: good enough?)
exprToPattern (Var n)       = return $ VarP n
exprToPattern (Pair e e')   = PairP <$> exprToPattern e <*> exprToPattern e'
exprToPattern (Succ e)      = SuccP <$> exprToPattern e
exprToPattern (Proj Post n) = return $ ProjP n
exprToPattern (App f e)     = patApp ==<< (exprToPattern f, exprToPattern e)
-- exprToPattern (Infty)    = return $ DotP Infty -- leads to non-term in compareExpr
exprToPattern _ = fail "exprToPattern"

-- | Only constructor patterns can be applied to a pattern.
patApp :: Pattern -> Pattern -> Maybe Pattern
patApp (ConP co n ps) p = Just $ ConP co n (ps ++ [p])
patApp _              _ = Nothing

-- | @exprToDotPat@ turns an expression into a pattern.
-- The @Bool@ is @True@ if the pattern is proper, i.e., does not contain
-- @DotP@ except @DotP Infty@.
exprToDotPat :: Expr -> (Bool, Pattern)
exprToDotPat = fromAllWriter . exprToDotPat'

exprToDotPat' :: Expr -> Writer All Pattern
exprToDotPat' e = do
  let fallback = tell (All False) >> return (DotP e)
  case e of
    Def (DefId (ConK co) n) -> return $ ConP pi n [] where
      pi = PatternInfo co False False -- not irrefutable (TODO: good enough?)
    Proj Post n -> return $ ProjP n
    Var n       -> return $ VarP n
    Pair e e'   -> PairP <$> exprToDotPat' e <*> exprToDotPat' e'
    Infty       -> return $ DotP Infty
    Succ e      -> SuccP <$> exprToDotPat' e
    App f e     -> maybe fallback return =<< do
      patApp <$> exprToDotPat' f <*> exprToDotPat' e
{-
    (App f e') -> do
      pf <- exprToDotPat' f
      case pf of
         (ConP co c ps) -> do pe <- exprToDotPat' e'
                              return $ ConP co c (ps ++ [pe])
         _ -> fallback
-}
    _ -> fallback

patternToExpr :: Pattern -> Expr
patternToExpr (VarP n)       = Var n
patternToExpr (SizeP m n)    = Var n
patternToExpr (ConP pi n ps) = List.foldl App (con (coPat pi) n) (List.map patternToExpr ps)
-- patternToExpr (ConP co n ps) = Con co n `App` (List.map patternToExpr ps)
patternToExpr (PairP p p')   = Pair (patternToExpr p) (patternToExpr p')
patternToExpr (SuccP p)      = Succ (patternToExpr p)
patternToExpr (UnusableP p)  = patternToExpr p
patternToExpr (ProjP n)      = Proj Post n
patternToExpr (DotP e)       = e -- cannot put Irr here because introPatType wants to compute the value of a dot pattern (after all bindings have been introduced)
patternToExpr (ErasedP p)    = erasedExpr $ patternToExpr p
patternToExpr (AbsurdP)      = Irr

-- | Dot all constructor subpatterns.  Used when expanding a dotted patsyn.
dotConstructors :: Pattern -> Pattern
dotConstructors p =
  case p of
    ConP pi c ps -> ConP pi{ dottedPat = True } c $ List.map dotConstructors ps
    PairP p1 p2  -> PairP (dotConstructors p1) (dotConstructors p2)
    _            -> p

-- admissible pattern ------------------------------------------------

-- completeP is used in admPattern, should not be True for UnusableP
completeP :: Pattern -> Bool
completeP (DotP _) = True
completeP (VarP _) = True
completeP SizeP{}  = False -- True
completeP (UnusableP p) = completeP p
completeP (ErasedP p)   = completeP p
completeP _ = False

isDotPattern :: Pattern -> Bool
isDotPattern (DotP _ ) = True
isDotPattern _ = False

-- isSuccessorPattern is used in admPattern, should not be True for UnusableP
isSuccessorPattern :: Pattern -> Bool
isSuccessorPattern (SuccP _)   = True
isSuccessorPattern (DotP e)    = isSuccessor e
isSuccessorPattern (ErasedP p) = isSuccessorPattern p
isSuccessorPattern _ = False

isSuccessor :: Expr -> Bool
isSuccessor (Ann e)  = isSuccessor (unTag e)
isSuccessor (Succ e) = True
isSuccessor _        = False

shallowSuccP :: Pattern -> Bool
shallowSuccP p = case p of
     (SuccP p)   -> isVarP p
     (ErasedP p) -> shallowSuccP p
     (DotP e)    -> shallowSuccE e
     _           -> False

   where isVarP (VarP _)         = True
         isVarP (DotP e)         = isVarE e
         isVarP (ErasedP p)      = isVarP p
         isVarP _                = False

         isVarE (Ann e)          = isVarE (unTag e)
         isVarE (Var _)          = True
         isVarE _                = False

         shallowSuccE (Ann e)    = shallowSuccE (unTag e)
         shallowSuccE (Succ e)   = isVarE e
         shallowSuccE _          = False

-- telescopes --------------------------------------------------------

---- construction

-- | typeToTele ((x : A) -> (y : B) -> C) = ([(x,A),(y,B)], C)
typeToTele :: Type -> (Telescope, Type)
typeToTele = typeToTele' (-1) -- take all Pis into the telescope

-- | @typeToTele' k t@.
--   If @k > 0@ it takes at most @k@ leading @Pi@s into the telescope
--   STALE: (hidden bindings do not count).
typeToTele' :: Int -> Type -> (Telescope, Type)
typeToTele' k t = mapFst Telescope $ ttt k t []
    where
      ttt :: Int -> Type -> [TBind] -> ([TBind], Type)
--      ttt k (Quant Pi htel tb t2) tel | k /= 0 = ttt (k-1) t2 (telescope htel ++ tb : tel)
      ttt k (Quant Pi tb t2) tel | k /= 0 = ttt (k-1) t2 (tb : tel)
      ttt k t tel = (reverse tel, t)

---- modification

instance LensDec Telescope where
  getDec   = error "getDec not defined for Telescope"
  mapDec f = Telescope . List.map (mapDec f) . telescope

---- destruction

teleLam :: Telescope -> Expr -> Expr
teleLam tel e = foldr (uncurry Lam) e $
  List.map (\ tb -> (decor $ boundDom tb, boundName tb)) $ telescope tel

teleToType' :: (Dec -> Dec) -> Telescope -> Type -> Type
teleToType' mod tel t = foldr (\ tb -> pi (mapDec mod tb)) t $ telescope tel
{-
teleToType' mod []       t = t
teleToType' mod (tb:tel) t = Pi (mapDec mod tb) (teleToType' mod tel t)
-}

teleToType :: Telescope -> Type -> Type
teleToType = teleToType' id

teleToTypeErase :: Telescope -> Type -> Type
teleToTypeErase = teleToType' demote -- (\ dec -> dec { erased = True })

adjustTopDecs :: (Dec -> Dec) -> Type -> Type
adjustTopDecs f t = teleToType' f tel core where
  (tel, core) = typeToTele t

teleToTypeM :: (Applicative m) => (Dec -> m Dec) -> Telescope -> Type -> m Type
teleToTypeM mod tel t =
  foldr (\ tb mt -> pi <$> mapDecM mod tb <*> mt) (pure t) $ telescope tel

adjustTopDecsM :: (Applicative m) => (Dec -> m Dec) -> Type -> m Type
adjustTopDecsM f t = teleToTypeM f tel core where
  (tel, core) = typeToTele t


{- How to translate a clause with patterns into one that does irrefutable
   matching on records

f (zero, (x, (y, z))) true (x', false) = rhs

 translates to

f (zero, xyz) true (x', false) rhs'  where rhs = subst
  [ fst xyz       / x,
    fst (snd xyz) / y,
    snd (snd xyz) / z,
    x' / x'
  ] rhs'

We walk through the patterns from left to right, to get the de Bruijn indices
for the pattern variables (dot patterns also have a de Bruijn index).

  Gamma, pi, n |- x --> Gamma(pi(n)), n+1, [n/n]

  Gamma, pi, n |- .t --> infer

If we return from a record pattern whose components were all irrefutable, we
apply a substitution to Telescope


-}

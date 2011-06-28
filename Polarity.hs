{- In the context of polarities, we use "recursive" in the sense of
"computable" rather than syntactic recursion. -}

module Polarity where

import Util
import Warshall

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

{- 2010-10-09 Fusing polarity and irrelevance

     .      constant (= irrelevant) function
    / \
  ++   |    strictly positive function (types only)
   |   |
   +   -    positive/negative function (types only)
    \ /
     ^      parametric function (lambda cube), default for types 
     |
     *      recursive function (pattern matching), default for terms


 Composition (AC)     
                      
  . p = .             
  * p = *  (p not .)  
  ^ p = ^  (p not .,*)
 ++ p = p             
  + p = p  (p not ++) 
  - - = +             

Equality/subtyping <=p

  x <=. y  iff  true
  x <=- y  iff  x >= y
  x <=^ y  iff  x == y
  x <=* y  iff  x == y 
 -}

-- polarities and strict positivity ----------------------------------

class Polarity pol where
  erased    :: pol -> Bool
  compose   :: pol -> pol -> pol
  neutral   :: pol                 -- ^ neutral for compose.
  promote   :: pol -> pol
  demote    :: pol -> pol

type PVarId = Int

data Pol 
  = Const -- non-occurring, irrelevant
  | SPos  -- strictly positive 
  | Pos   -- positive
  | Neg   -- negative, used internally for contravariance of sized codata  
  | Param -- parametric (lambda) function
  | Rec   -- recursive (takes decision)
  | Default     -- no polarity given (for parsing)
  | PVar PVarId -- flexible polarity variable
         deriving (Eq,Ord)

mixed = Rec
defaultPol = Rec
{-
mixed = Param -- TODO: Rec
defaultPol = Param -- TODO: Rec
-}
instance Polarity Pol where
  erased  = (==) Const
  compose = polComp
  neutral = SPos
  promote = invComp Const
  demote  = invComp Rec

instance Show Pol where
  show Const = "."
  show SPos  = "++"
  show Pos   = "+"
  show Neg   = "-"
  show Param = "^"
  show Rec   = "*"
  show Default = "{default polarity}"
  show (PVar i) = showPVar i

showPVar i = "?p" ++ show i

isPVar (PVar{}) = True
isPVar _ = False

-- information ordering
leqPol :: Pol -> Pol -> Bool
leqPol x Const  = True   -- Const is top
leqPol Const x  = False
leqPol Rec y    = True   -- Rec is bottom
leqPol x Rec    = False
leqPol Param y  = True   -- Param is second bottom
leqPol x Param  = False
leqPol Pos SPos = True 
leqPol x   y    = x == y

{- RETIRED
isSPos :: Pol -> Bool
isSPos SPos = True
isSPos Const = True
isSPos _ = False
-}

{- NOT USED
isPos :: Pol -> Bool
isPos Pos = True
isPos x = isSPos x
-}

-- polarity negation
-- used in Eval.hs leqVals' for switching sides
-- this means it is only applied to Pos, Neg, Param, 
-- never to SPos, Const, or polarity expressions
polNeg :: Pol -> Pol
polNeg Const  = Const
polNeg SPos  = Neg
polNeg Pos   = Neg
polNeg Neg   = Pos
polNeg Param = Param
polNeg Rec   = Rec

-- polarity composition
-- used in Eval.hs leqVals'
polComp :: Pol -> Pol -> Pol
polComp Const  x  = Const   -- most dominant
polComp x Const   = Const
polComp Rec x     = Rec  -- dominant except for Const
polComp x Rec     = Rec
polComp Param x   = Param  -- dominant except for Const, Rec
polComp x Param   = Param
polComp SPos  x   = x      -- neutral
polComp x SPos    = x
polComp Pos  x    = x      -- neutral except for SPos
polComp x Pos     = x
polComp Neg Neg   = Pos    -- order 2
{- pol.comp. is ass., comm., with neutral ++, and infinity Const
   cancellation does not hold, since composition with anything by ++ is
   information loss:
     q p <= q p' ==> p <= p'
   only if q = ++ (then it is trivial anyway) -}

-- polarity inverse composition (see Abel, MSCS 2008)
-- invComp p q1 <= q2  <==> q1 <= polComp p q2 
-- used in TCM.hs cxtApplyDec
invComp :: Pol -> Pol -> Pol
invComp Rec   Rec   = Rec       -- in rec. arg. keep only rec. vars
invComp Rec   x     = Const     -- all others are declared unusable
invComp Param Param = Param     -- in parametric mixed arg, keep only mixed vars
invComp Param x     = Const  
invComp Const x     = Param     -- a constant function can take any argument
invComp SPos  x     = x         -- SPos is the identity
invComp p     SPos  = Const     -- SPos preserved only under SPos
invComp Pos   x     = x         -- x not SPos
invComp Neg   x     = polNeg x  -- x not SPos

{- UNUSED
invCompExpr :: Pol -> PExpr -> PExpr
invCompExpr q (PValue p)   = PValue $ invComp q p
invCompExpr q (PExpr q' i) = PExpr (polComp q q') i 
-}

-- polarity conjuction (infimum)
-- used in comparing spines
polAnd :: Pol -> Pol -> Pol
polAnd Const x = x      -- most information
polAnd x Const = x
polAnd Rec   x = Rec   -- least information
polAnd x   Rec = Rec
{-
polAnd Param x  = Param   -- 2nd least information
polAnd x Param  = Param
-}
polAnd x y | x == y = x       -- same information
polAnd SPos Pos = Pos     -- SPos is more informative than Pos
polAnd Pos SPos = Pos
{-
polAnd SPos Neg = Param
polAnd Neg SPos = Param
-}
polAnd _ _      = Param     -- remaining cases: conflicting info or Param

instance SemiRing Pol where
  oplus  = polAnd
  otimes = polComp
  ozero  = Const    -- dominant for composition, neutral for infimum
  oone   = SPos     -- neutral  for composition

-- computing a relation from <=
relPol :: Pol -> (a -> a -> Bool) -> (a -> a -> Bool)
relPol Const r a b = True
relPol Rec   r a b = r a b && r b a
relPol Param r a b = r a b && r b a
relPol Neg   r a b = r b a
relPol Pos   r a b = r a b
relPol SPos  r a b = r a b

relPolM :: (Monad m) => Pol -> (a -> a -> m ()) -> (a -> a -> m ())
relPolM Const r a b = return ()
relPolM Rec   r a b = r a b >> r b a
relPolM Param r a b = r a b >> r b a
relPolM Neg   r a b = r b a
relPolM Pos   r a b = r a b
relPolM SPos  r a b = r a b

-- polarity product (composition of polarities) ----------------------

data Multiplicity = POne | PTwo deriving (Eq, Ord)

instance Show Multiplicity where
  show POne = "1"
  show PTwo = "2"

-- addition modulo 2
addMultiplicity :: Multiplicity -> Multiplicity -> Multiplicity
addMultiplicity PTwo y = y
addMultiplicity x PTwo = x
addMultiplicity POne POne = PTwo

type VarMults = Map PVarId Multiplicity -- multiplicity of variables (1 or 2) 

showMults :: VarMults -> String
showMults mults = 
  let ml = Map.toList mults  -- get list of (key,value) pairs
      l  = concat $ map f ml where
             f (k, POne) = [k]
             f (k, PTwo) = [k,k]
  in Util.showList "." showPVar l    

multsEmpty = Map.empty

multsSingle :: Int -> VarMults
multsSingle i = Map.insert i POne multsEmpty


data PProd = PProd 
  { coeff    :: Pol      -- a coefficient, excluding PVar
  , varMults :: VarMults -- multiplicity of variables (1 or 2) 
  } deriving (Eq,Ord)

instance Polarity PProd where
  erased  = erased . coeff 
  compose = polProd
  neutral = PProd SPos multsEmpty
  demote  = undefined
  promote = undefined

instance Show PProd where
  show (PProd Const _) = show Const
  show (PProd SPos m) = if Map.null m then show SPos else showMults m
  show (PProd q m) = separate "." (show q) (showMults m)

pprod :: Pol -> PProd
pprod (PVar i) = PProd SPos (multsSingle i)
pprod q = PProd q multsEmpty

-- | fails if not a simple polarity
fromPProd :: PProd -> Maybe Pol
fromPProd (PProd Const _)          = Just Const
fromPProd (PProd p m) | Map.null m = Just p
fromPProd _                        = Nothing

isSPos :: PProd -> Bool
isSPos (PProd Const _) = True
isSPos (PProd SPos m) = Map.null m
isSPos _ = False

-- multiply two products

polProd :: PProd -> PProd -> PProd
polProd (PProd q1 m1) (PProd q2 m2) = PProd (polComp q1 q2) $
  Map.unionWith addMultiplicity m1 m2

-- polarity expressions are polynomials ------------------------------

data PPoly = PPoly { monomials :: [PProd] } deriving (Eq,Ord)

instance Show PPoly where
  show (PPoly []) = show Const
  show (PPoly [m]) = show m
  show (PPoly l)   = Util.showList "/\\" show l

ppoly :: PProd -> PPoly
ppoly (PProd Const _) = PPoly []
ppoly pp = PPoly [pp]

polSum :: PPoly -> PPoly -> PPoly
polSum (PPoly x) (PPoly y) = PPoly $ List.nub $ x ++ y

polProduct :: PPoly -> PPoly -> PPoly
polProduct (PPoly l1) (PPoly l2) =
  let ps = [ polProd x y | x <- l1, y <- l2]
  in PPoly $ List.nub $ ps
 
instance SemiRing PPoly where
  oplus  = polSum
  otimes = polProduct
  ozero  = PPoly []
  oone   = PPoly [PProd SPos Map.empty]

{-
data PExpr 
  = PValue Pol     -- constant polarity
  | PExpr Pol Int  -- PExpr q pi means q^_1 pi  (pi is the number of the var)

-- a polarity variable
pvar :: Int -> PExpr
pvar = PExpr SPos  -- ++ is the neutral element of inverse polarity composition
  
instance Show PExpr where
  show (PValue p) = show p
  show (PExpr SPos i) = "?p" ++ show i
  show (PExpr q i) = show q ++ "^-1(?p" ++ show i ++ ")"
-}


{- ML-style Polarity inference

Preliminaries:
1. constructor types are mixed-variant function types only
2. matching is only allowed on mixed-variant arguments
  1+2 are both consequences that only type-valued functions have variance
  and 1. data constructors are not types, 2. types are not matched on

Concrete syntax

  f : (xs : As) -> C   (C not a Pi-type)
  f = t

is parsed as abstract syntax

  f : pis(xs : As) -> C
  f = t

where pi_1..n are fresh polarity variables

Then t is type-checked to infer the polarity variables, e.g.

  f xs = t

  pis(xs : As) |- t : C

Now what can happen?

Variable:  t = x_i.  Then we add a constraint  pi_i <= ++

Application t = u v  where u : q(x:B) -> D

  q^-1(pis(xs: As)) |- v : B

  A term q^-1 pi arises where q is a polarity constant (!, ML-inference)
  or a polarity variable (recursion!, e.g. u = f)
  and pi is a polarity expression

In the context, keep SOLL and HABEN

  SOLL  is the original polarity (variable or constant)
  HABEN is a (ordered) list of pol.vars. and a pol.const. (default: ++)

Variable   : add constraint SOLL <= HABEN
Application: add q to HABEN by polarity multiplication (q is a var or const)
Abstraction: \xt : q(x:A) -> B:  continue with x (SOLL = q, HABEN = ++)

What kind of constraints do arise
1) q  <= pi    [ from variables , pi is a Pol-product ]
2) ++ <= pis  [ from positivity graph, pis is a sum of Pol-products ]
   this means ++ <= pi for all pi in pis

Solving constraints
 
- discard  o <= pi  and q <= /  (do not even need to add them)
- all pvars which are not bounded below (appearing in one q in 1)
  can be instantiated to /  which will remove some constraints


-}

{- Mutual recursion 

In mutual declarations, use the following Ansatz:  data/codata ++, functions o

  A = B -> A
  B = A -> B

A (B) is positive in its own body and negative in the body of B (A)

  F A B = B -> A   F(-,++)
  G A B = A -> B   G(-,++)

  F A B = G A B -> F A B  
  G A B = F A B -> G A B

  Polarities: 
  F : fa * -> fb * -> *
  G : ga * -> gb * -> *  

  A : -fa, B : -fb |- G A B : *  ==> -fa <= ga, -fb <= gb
  A : -ga, B : -gb |- F A B : *  ==> -ga <= fa, -gb <= fb

-}

{- Pure polarity inference

Judgement:  pis(xs:As) |- t : B ---> C

Variable:   pis(xs:As) |- xi : Ai ---> pi_i <= ++

Application: Delta |- u : q(x:A) -> B ---> C1
             Delta |- v : A           ---> C2
             --------------------------------------------------
             Delta |- u v : B[u/x] ---> C1,C2,q(Delta) <= Delta
-}
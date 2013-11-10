{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Value where

import Prelude hiding (null)

import Control.Applicative

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

import Abstract
import Polarity
import Util
import TraceError -- orM

-- call-by-value
-- cofuns are not forced

data Val
  -- sizes
  = VInfty
  | VZero
  | VSucc Val
  | VMax [Val]
  | VPlus [Val]
  | VMeta MVar Env Int           -- X rho + n  (n-fold successor of X rho)
  -- types
  | VSort (Sort Val)
  | VMeasured (Measure Val) Val  -- mu -> A  (only in checkPattern)
  | VGuard (Bound Val) Val       -- mu<mu' -> A
  | VBelow LtLe Val              -- domain in bounded size quant.
  | VQuant
    { vqPiSig :: PiSigma
    , vqName  :: Name
    , vqDom   :: Domain
    , vqFun   :: FVal
    }
  | VSing Val TVal               -- Singleton type (TVal not Pi)
  -- functions
  | VLam Name Env Expr
  | VAbs Name Int Val Valuation  -- abstract free variable
  | VConst Val                   -- constant function
  | VUp Val TVal                 -- delayed eta expansion; TVal is a Pi
  -- values
  | VRecord RecInfo EnvMap       -- a record value / fully applied constructor
  | VPair Val Val                -- eager pair
  -- neutrals
  | VGen Int                     -- free variable (de Bruijn level)
  | VDef DefId                   -- co(data/constructor/fun)
                                 -- VDef occurs only inside a VApp!
  | VCase Val TVal Env [Clause]
  | VApp Val [Clos]
  -- closures
  | VProj PrePost Name           -- a projection as an argument to a neutral
  | VClos Env Expr               -- closure for cbn evaluation
  -- don't care
  | VIrr                         -- erased hypothetical inhabitant of empty type
    deriving (Eq,Ord)

-- | Makes constant function if name is empty.
vLam :: Name -> Env -> Expr -> FVal
vLam x env e
  | emptyName x = VConst $ VClos env e
  | otherwise   = VLam x env e

-- | Is a value a function?  May become more @True@ after forcing the @VUp@.
isFun :: Val -> Bool
isFun VLam{}                         = True
isFun VAbs{}                         = True
isFun VConst{}                       = True
isFun (VUp _ VQuant{ vqPiSig = Pi }) = True
isFun v                              = False

absName :: FVal -> Name
absName fv =
  case fv of
    VLam x _ _              -> x
    VAbs x _ _ _            -> x
    VUp _ (VQuant Pi x _ _) -> x
    _                       -> noName

type FVal = Val
type TVal = Val -- type value
type Clos = Val
type Domain = Dom TVal

-- | Valuation of free variables.
newtype Valuation = Valuation { valuation :: [(Int,Val)] }
  deriving (Eq,Ord)

emptyVal  = Valuation []
sgVal i v = Valuation [(i,v)]

valuateGen :: Int -> Valuation -> Val
valuateGen i valu = maybe (VGen i) id $ lookup i $ valuation valu

type TeleVal = [TBinding Val]

data Environ a = Environ
  { envMap   :: [(Name,a)]          -- the actual map from names to values
  , envBound :: Maybe (Measure Val) -- optionally the current termination measure
  }
               deriving (Eq,Ord,Show)

type EnvMap = [(Name,Val)]
type Env = Environ Val

{-
data MeasVal = MeasVal [Val]  -- lexicographic termination measure
               deriving (Eq,Ord,Show)
-}

-- smart constructors ------------------------------------------------

-- | The value representing type Size.
vSize :: Val
vSize = VBelow Le VInfty -- 2012-01-28 non-termination bug I have not found
-- vSize = VSort $ SortC Size

vFinSize = VBelow Lt VInfty

-- | Ensure we construct the correct value representing Size.
vSort :: Sort Val -> Val
vSort (SortC Size) = vSize
vSort s            = VSort s

isVSize :: Val -> Bool
isVSize (VSort (SortC Size)) = True
isVSize (VBelow Le VInfty)   = True
isVSize _                    = False

vTSize = VSort $ SortC TSize

vTopSort :: Val
vTopSort = VSort $ Set VInfty

mkClos :: Env -> Expr -> Val
mkClos rho Infty       = VInfty
mkClos rho Zero        = VZero
-- mkClos rho (Succ e)    = VSucc (mkClos rho e)  -- violates an invariant!! succeed/crazys
mkClos rho (Below ltle e) = VBelow ltle (mkClos rho e)
mkClos rho (Proj fx n) = VProj fx n
mkClos rho (Var x) = lookupPure rho x
mkClos rho (Ann e) = mkClos rho $ unTag e
mkClos rho e = VClos rho e
  -- Problem with MetaVars: freeVars of a meta var is unknown in this repr.!
  -- VClos (rho { envMap = filterEnv (freeVars e) (envMap rho)}) e

filterEnv :: Set Name -> EnvMap -> EnvMap
filterEnv ns [] = []
filterEnv ns ((x,v) : rho) =
  if Set.member x ns then (x,v) : filterEnv (Set.delete x ns) rho
   else filterEnv ns rho

vDef id   = VDef id `VApp` []
vCon co n = vDef $ DefId (ConK co) n
-- vCon co n = vDef $ DefId (ConK (coToConK co)) n
vFun n    = vDef $ DefId FunK $ QName n
vDat n    = vDef $ DefId DatK n

{- POSSIBLY BREAKS INVARIANT!
vApp :: Val -> [Val] -> Val
vApp f [] = f
vApp f vs = VApp f vs
-}

failValInv :: (Monad m) => Val -> m a
failValInv v = fail $ "internal error: value " ++ show v ++ " violates representation invariant"

vAbs :: Name -> Int -> Val -> FVal
vAbs x i v = VAbs x i v emptyVal

arrow , prod :: TVal -> TVal -> TVal
arrow = quant Pi
prod  = quant Sigma

quant piSig a b = VQuant piSig x (defaultDomain a) (VConst b)
  where x   = fresh ""
-- quant piSig a b = VQuant piSig x (defaultDomain a) (Environ [(bla,b)] Nothing) (Var bla)
--   where x   = fresh ""
--         bla = fresh "#codom"


-- * Sizes ------------------------------------------------------------

-- Sizes form a commutative semiring with multiplication (Plus) and
-- idempotent addition (Max)
--
-- Wellformed size values are polynomials, i.e., sums (Max) of products (Plus).
-- A monomial m takes one of the forms (k stands for a variable: VGen or VMeta)
-- 0. VSucc^* VZero
-- 1. VSucc^* k
-- 2. VSucc^* (VPlus [k1,...,kn])   where n>=2
-- A polynomial takes one of the forms
-- 0. VInfty
-- 1. m
-- 2. VMax ms  where length ms >= 2 and each mi different
{- OLD
-- * VSucc^* VGen
-- * VMax vs where each v_i = VSucc^* (VGen k_i) and all k_i different
--           and vs has length >= 2
-}
--
-- the smart constructors construct wellformed size values using the laws
-- $ #             = #                Infty
-- max # k         = #
-- $ (max i j)     = max ($ i) ($ j)  $ distributes over max
-- max (max i j) k = max i j k        Assoc-Commut of max
-- max i i         = i                Idempotency of max
succSize :: Val -> Val
succSize v = case v of
            VInfty -> VInfty
            VMax vs -> maxSize $ map succSize vs
            VMeta i rho n -> VMeta i rho (n + 1)  -- TODO: integrate + and mvar
            _ -> VSucc v
vSucc = succSize

-- "multiplication" of sizes
plusSize :: Val -> Val -> Val
plusSize VZero v = v
plusSize v VZero = v
plusSize VInfty v = VInfty
plusSize v VInfty = VInfty
plusSize (VMax vs) v = maxSize $ map (plusSize v) vs
plusSize v (VMax vs) = maxSize $ map (plusSize v) vs
plusSize (VSucc v) v' = succSize $ plusSize v v'
plusSize v' (VSucc v) = succSize $ plusSize v v'
plusSize (VPlus vs) (VPlus vs') = VPlus $ List.sort (vs ++ vs') -- every summand is a var!  -- TODO: more efficient sorting!
plusSize (VPlus vs) v = VPlus $ List.insert v vs
plusSize v (VPlus vs) = VPlus $ List.insert v vs
plusSize v v' = VPlus $ List.sort [v,v']

plusSizes :: [Val] -> Val
plusSizes [] = VZero
plusSizes [v] = v
plusSizes (v:vs) = v `plusSize` (plusSizes vs)

-- maxSize vs = VInfty                 if any v_i=Infty
--            = VMax (sort (nub (flatten vs)) else
-- precondition vs

maxSize :: [Val] -> Val
maxSize vs = case Set.toList . Set.fromList <$> flatten vs of
   Nothing -> VInfty
   Just [] -> VZero
   Just [v] -> v
   Just vs' -> VMax vs'
  where flatten (VZero:vs) = flatten vs
        flatten (VInfty:_) = Nothing
        flatten (VMax vs:vs') = flatten vs' >>= return . (vs++)
        flatten (v:vs) = flatten vs >>= return . (v:)
        flatten [] = return []

{-
maxSize :: [Val] -> Val
maxSize vs = case flatten [] vs of
   [] -> VInfty
   [v] -> v
   vs' -> VMax vs'
  where flatten acc (VInfty:_) = []
        flatten acc (VMax vs:vs') = flatten (vs ++ acc) vs'
        flatten acc (v:vs) = flatten (v:acc) vs
        flatten acc [] = Set.toList $ Set.fromList acc -- sort, nub
-}

-- * destructors -------------------------------------------------------

vSortToSort :: Sort Val -> Sort Expr
vSortToSort (SortC c)    = SortC c
vSortToSort (Set VInfty) = Set Infty

predSize :: Val -> Maybe Val
predSize VInfty = Just VInfty
predSize (VSucc v) = Just v
predSize (VMax vs) = do vs' <- mapM predSize vs
                        return $ maxSize vs'
predSize (VMeta v rho n) | n > 0 = return $ VMeta v rho (n-1)
predSize _ = Nothing -- variable or zero or sum

instance HasPred Val where
  predecessor VInfty = Nothing -- for printing bounds
  predecessor v = predSize v

isFunType :: TVal -> Bool
isFunType VQuant{ vqPiSig = Pi } = True
isFunType _                      = False

isDataType :: TVal -> Bool
isDataType (VApp (VDef (DefId DatK _)) _) = True
isDataType (VSing v tv) = isDataType tv
isDataType _ = False

-- * ugly printing -----------------------------------------------------

instance Show (Sort Val) where
  show (SortC c) = show c
  show (Set VZero) = "Set"
  show (CoSet VInfty) = "Set"
  show (Set v) = parens $ ("Set " ++ show v)
  show (CoSet v) = parens $ ("CoSet " ++ show v)

instance Show Val where
  show v | isVSize v = "Size"
  show (VSort s) = show s
  show VInfty = "#"
  show VZero = "0"
  show (VSucc v) = "($ " ++ show v ++ ")"
  show (VMax vl) = "(max " ++ showVals vl ++ ")"
  show (VPlus (v:vl)) = parens $ foldr (\ v s -> show v ++ " + " ++ s) (show v) vl
  show (VApp v []) = show v
  show (VApp v vl) = "(" ++ show v ++ " " ++ showVals vl ++ ")"
  show (VDef id) = show id
  show (VProj Pre id) = show id
  show (VProj Post id) = "." ++ show id
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show (VGen k) = "v" ++ show k
  show (VMeta k rho 0) = "?" ++ show k ++ showEnv rho
  show (VMeta k rho 1) = "$?" ++ show k ++ showEnv rho
  show (VMeta k rho n) = "(?" ++ show k ++ showEnv rho ++ " + " ++ show n ++")"
  show (VRecord ri env) = show ri ++ "{" ++ Util.showList "; " (\ (n, v) -> show n ++ " = " ++ show v) env ++ "}"
  show (VCase v vt env cs) = "case " ++ show v ++ " : " ++ show vt ++ " { " ++ showCases cs ++ " } " ++ showEnv env
  show (VClos (Environ [] Nothing) e) = showsPrec precAppR e ""
  show (VClos env e) = "{" ++ show e ++ " " ++ showEnv env ++ "}"
  show (VSing v vt) = "<" ++ show v ++ " : " ++ show vt ++ ">"
  show VIrr  = "."
  show (VMeasured mu tv) = parens $ show mu ++ " -> " ++ show tv
  show (VGuard beta tv) = parens $ show beta ++ " -> " ++ show tv
  show (VBelow ltle v) = show ltle ++ " " ++ show v

  show (VQuant pisig x (Domain (VBelow ltle v) ki dec) bv)
       | (ltle,v) /= (Le,VInfty) =
       parens $ (\ p -> if p==defaultPol then "" else show p) (polarity dec) ++
                (if erased dec then brackets binding else parens binding)
                 ++ " " ++ show pisig ++ " " ++ showSkipLambda bv
            where binding = show x ++ " " ++ show ltle ++ " " ++ show v

  show (VQuant pisig x (Domain av ki dec) bv) =
        parens $ (\ p -> if p==defaultPol then "" else show p) (polarity dec) ++
                (if erased dec then brackets binding
                  else if emptyName x then s1 else parens binding)
                    ++ " " ++ show pisig ++ " " ++ showSkipLambda bv
             where s1 = s2 ++ s0
                   s2 = show av
                   s3 = show ki
                   s0 = if ki == defaultKind || s2 == s3 then "" else "::" ++ s3
                   binding = if emptyName x then  s1 else show x ++ " : " ++ s1

  show (VLam x env e) = "(\\" ++ show x ++ " -> " ++ show e ++ showEnv env ++ ")"
  show (VConst v) = "(\\ _ -> " ++ show v ++ ")"
  show (VAbs x i v valu) = "(\\" ++ show x ++ "@" ++ show i ++ show v ++ showValuation valu ++ ")"
  show (VUp v vt) = "(" ++ show v ++ " Up " ++ show vt ++ ")"

showSkipLambda v =
  case v of
    (VLam x env e)    -> show e ++ showEnv env
    (VConst v)        -> show v
    (VAbs x i v valu) -> show v ++ showValuation valu
    v                 -> show v

showVals :: [Val] -> String
showVals [] = ""
showVals (v:vl) = show v ++ (if null vl then "" else " " ++ showVals vl)

-- environment ---------------------------------------------------

emptyEnv :: Environ a
emptyEnv = Environ [] Nothing

appendEnv :: Environ a -> Environ a -> Environ a
appendEnv (Environ rho mmeas) (Environ rho' mmeas') =
  Environ (rho ++ rho') (orM mmeas mmeas')

-- | enviroment extension / update
update :: Environ a -> Name -> a -> Environ a
update env n v | emptyName n = env
               | otherwise   = env { envMap = (n,v) : envMap env }

lookupPure :: Show a => Environ a -> Name -> a
lookupPure rho x =
    case lookup x (envMap rho) of
      Just v -> v
      Nothing -> error $ "lookupPure: unbound identifier " ++ show x ++ " in environment " ++ show rho

lookupEnv :: Monad m => Environ a -> Name -> m a
lookupEnv rho x =
    case lookup x (envMap rho) of
      Just v -> return $ v
      Nothing -> fail $ "lookupEnv: unbound identifier " ++ show x --  ++ " in environment " ++ show rho
{-
lookupEnv :: Monad m => Environ a -> Name -> m a
lookupEnv [] n = fail $ "lookupEnv: identifier " ++ show n ++ " not bound"
lookupEnv ((x,v):xs) n = if x == n then return v
                          else lookupEnv xs n
-}

showValuation :: Valuation -> String
showValuation (Valuation [])  = ""
showValuation (Valuation tau) = "{" ++ Util.showList ", " (\(i,v) -> show i ++ " = " ++ show v) tau ++ "}"

showEnv :: Environ Val -> String
showEnv (Environ [] Nothing)   = ""
showEnv (Environ rho Nothing)  = "{" ++ showEnv' rho ++ "}"
showEnv (Environ [] (Just mu)) = "{ measure=" ++ show mu ++ " }"
showEnv (Environ rho (Just mu)) = "{" ++ showEnv' rho ++ " | measure=" ++ show mu ++ " }"

showEnv' :: EnvMap -> String
showEnv' = Util.showList ", " (\ (n,v) -> show n ++ " = " ++ show v)

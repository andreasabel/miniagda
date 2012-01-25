{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
 
module Value where

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
  | VQuant PiSigma Name Domain Env Expr     
  | VSing Val TVal               -- Singleton type (TVal is base type)
  -- functions                   
  | VLam Name Env Expr           
  | VUp Val TVal                 -- delayed eta expansion; TVal is a VPi
  -- neutrals                    
  | VGen Int                     -- free variable (de Bruijn level)
  | VDef DefId                   -- co(data/constructor/fun)
                                 -- VDef occurs only inside a VApp!
  | VCase Val Env [Clause]       
  | VApp Val [Clos]
  | VRecord EnvMap               -- a record value             
  | VProj Name                   -- a projection as an argument to a neutral 
  | VPair Val Val                -- eager pair
  | VClos Env Expr               -- closure for cbn evaluation
  -- don't care                  
  | VIrr                         -- erased hypothetical inhabitant of empty type
    deriving (Eq,Ord)

type TVal = Val
type Clos = Val
type Domain = Dom TVal

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
-- vSize = VBelow Le VInfty 
vSize = VSort $ SortC Size

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
mkClos rho (Proj n) = VProj n
mkClos rho (Var x) = lookupPure rho x 
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
vFun n    = vDef $ DefId FunK n
vDat n    = vDef $ DefId DatK n

{- POSSIBLY BREAKS INVARIANT!
vApp :: Val -> [Val] -> Val
vApp f [] = f
vApp f vs = VApp f vs
-}

failValInv :: (Monad m) => Val -> m a
failValInv v = fail $ "internal error: value " ++ show v ++ " violates representation invariant"

arrow :: TVal -> TVal -> TVal
arrow a b = VQuant Pi x (defaultDomain a) (Environ [(bla,b)] Nothing) (Var bla)
  where x   = fresh ""
        bla = fresh "#ARROW#"

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
            VMax vs -> VMax $ map succSize vs
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
maxSize vs = case flatten vs of
   Nothing -> VInfty
   Just [] -> VZero
   Just [v] -> v
   Just vs' -> VMax $ Set.toList $ Set.fromList $ vs' -- sort, nub
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

-- destructors -------------------------------------------------------

vSortToSort :: Sort Val -> Sort Expr
vSortToSort (SortC c)    = SortC c
vSortToSort (Set VInfty) = Set Infty
-- rest 

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
isFunType (VQuant Pi _ _ _ _) = True
isFunType _ = False

isDataType :: TVal -> Bool
isDataType (VApp (VDef (DefId DatK _)) _) = True
isDataType (VSing v tv) = isDataType tv
isDataType _ = False

-- ugly printing -----------------------------------------------------

instance Show (Sort Val) where
  show (SortC c) = show c
  show (Set VZero) = "Set"
  show (CoSet VInfty) = "Set"
  show (Set v) = parens $ ("Set " ++ show v)
  show (CoSet v) = parens $ ("CoSet " ++ show v)

instance Show Val where
    show = showVal

showVal :: Val -> String
showVal v | isVSize v = "Size"
--showVal (VSort (SortC c)) = show c
showVal (VSort s) = show s
showVal VInfty = "#"
showVal VZero = "0"
showVal (VSucc v) = "($ " ++ showVal v ++ ")" 
showVal (VMax vl) = "(max " ++ showVals vl ++ ")"
showVal (VPlus (v:vl)) = parens $ foldr (\ v s -> showVal v ++ " + " ++ s) (showVal v) vl 
showVal (VApp v []) = showVal v
showVal (VApp v vl) = "(" ++ showVal v ++ " " ++ showVals vl ++ ")"
-- showVal (VCon _ n) = n
showVal (VDef id) = show id -- show $ name id
showVal (VProj id) = "." ++ show id
showVal (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")" 
showVal (VGen k) = "v" ++ show k
showVal (VMeta k rho 0) = "?" ++ show k ++ showEnv rho
showVal (VMeta k rho 1) = "$?" ++ show k ++ showEnv rho
showVal (VMeta k rho n) = "(?" ++ show k ++ showEnv rho ++ " + " ++ show n ++")"
showVal (VRecord env) = "{" ++ Util.showList "; " (\ (n, v) -> show n ++ " = " ++ showVal v) env ++ "}"
showVal (VCase v env cs) = "case " ++ showVal v ++ "{ " ++ showCases cs ++ " } " ++ showEnv env 
showVal (VLam x env e) = "(\\" ++ show x ++ " -> " ++ show e ++ showEnv env ++ ")" 
showVal (VClos (Environ [] Nothing) e) = showsPrec precAppR e ""
showVal (VClos env e) = "{" ++ show e ++ " " ++ showEnv env ++ "}" 
showVal (VUp v vt) = "(" ++ show v ++ " Up " ++ show vt ++ ")" 
showVal (VSing v vt) = "<" ++ show v ++ " : " ++ show vt ++ ">" 
showVal VIrr  = "."
showVal (VMeasured mu tv) = parens $ show mu ++ " -> " ++ show tv
showVal (VGuard beta tv) = parens $ show beta ++ " -> " ++ show tv
showVal (VBelow ltle v) = show ltle ++ " " ++ show v

showVal (VQuant pisig x (Domain (VBelow ltle v) ki dec) env b) =
  parens $ (\ p -> if p==defaultPol then "" else show p) (polarity dec) ++
            (if erased dec then brackets binding else parens binding) 
              ++ " " ++ show pisig ++ show " " ++ show b ++ showEnv env
         where binding = show x ++ " " ++ show ltle ++ " " ++ showVal v

showVal (VQuant pisig x (Domain av ki dec) env b) =
  parens $ (\ p -> if p==defaultPol then "" else show p) (polarity dec) ++
            (if erased dec then brackets binding
              else if emptyName x then s1 else parens binding) 
                ++ " " ++ show pisig ++ " " ++ show b ++ showEnv env
         where s1 = s2 ++ s0
               s2 = showVal av
               s3 = show ki
               s0 = if ki == defaultKind || s2 == s3 then "" else "::" ++ s3 
               binding = if emptyName x then  s1 else show x ++ " : " ++ s1 

{-
showVal (VPi dec "" av env e) = "(" ++ showVal av ++ " -> " ++ show e ++ showEnv env ++ ")"
showVal (VPi dec x av env e) = "((" ++ x  ++ " : " ++ showVal av ++ ") -> " ++ show e ++ showEnv env ++ ")"
-}

showVals :: [Val] -> String
showVals [] = ""
showVals (v:vl) = showVal v ++ (if null vl then "" else " " ++ showVals vl)   

-- environment ---------------------------------------------------

emptyEnv :: Environ a
emptyEnv = Environ [] Nothing

appendEnv :: Environ a -> Environ a -> Environ a
appendEnv (Environ rho mmeas) (Environ rho' mmeas') = 
  Environ (rho ++ rho') (orM mmeas mmeas')

-- enviroment extension / update
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

showEnv :: Environ Val -> String
showEnv (Environ [] Nothing)   = ""
showEnv (Environ rho Nothing)  = "{" ++ showEnv' rho ++ "}"
showEnv (Environ [] (Just mu)) = "{ measure=" ++ show mu ++ " }"
showEnv (Environ rho (Just mu)) = "{" ++ showEnv' rho ++ " | measure=" ++ show mu ++ " }" 

showEnv' :: EnvMap -> String 
showEnv' = Util.showList ", " (\ (n,v) -> show n ++ " = " ++ showVal v)
{-
showEnv' ((n,v):env) = n ++ " = " ++ showVal v ++ 
      (if null env then "" else ", " ++ showEnv' env) 
-}

{-
fresh :: Environ a -> Name
fresh env = "fresh#" ++ show (length (envMap env))
-}
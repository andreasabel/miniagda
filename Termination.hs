{-# LANGUAGE ImplicitParams, PatternGuards #-}

module Termination where

import Data.Monoid
import Control.Monad.Writer

import Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

import Debug.Trace

--import System

import Abstract
import TraceError
import Util

import Semiring
import qualified SparseMatrix as M

import TreeShapedOrder (TSO)
import qualified TreeShapedOrder as TSO

traceTerm msg a = a -- trace msg a 
traceTermM msg = return () -- traceM msg
{-
traceTerm msg a = trace msg a 
traceTermM msg = traceM msg
-}


traceProg msg a =  a 
traceProgM msg = return ()
{-
traceProg msg a = trace msg a 
traceProgM msg = traceM msg
-}

-- cutoff:  How far can we count?
-- cutoff = 0 : decrease of -infty,0,1 (original SCT)
-- cutoff = 1 : "           -infty,-1,0,1,2
-- etc.
-- this is a parameter to the termination checker

cutoff :: Int
cutoff = 2  -- we can trace descend of 3, ascend of 2


type Matrix a = M.Matrix Int a

empty :: Matrix a
empty = M.M (M.Size 0 0) []

-- greater numbers shall mean more information for the term.checker.
data Order = Decr Int -- positive numbers: decrease, neg. numbers: increase
           | Un       -- infinite increase (- infty)
           | Mat (Matrix Order) -- square matrices only (rows = call arguments, cols = parameters of caller)
           deriving (Show,Eq,Ord)

instance HasZero Order where
  zeroElement = Un

-- smart constructor 
orderMat :: Matrix Order -> Order
orderMat m | M.isEmpty m                = Decr 0
           | Just o <- M.isSingleton m  = o 
           | otherwise                  = Mat m
{-   
orderMat []    = Decr 0   -- 0x0 Matrix = neutral element
orderMat [[o]] = o        -- 1x1 Matrix
orderMat oss   = Mat oss  -- nxn Matrix
-}

-- smart constructor
decr :: (?cutoff :: Int) => Int -> Order
decr i | i < - ?cutoff = Un
       | i > ?cutoff  = Decr (?cutoff + 1)
       | otherwise   = Decr i

-- present order in terms of <,<=,?
abstract :: Order -> Order
abstract (Decr k) | k > 0 = Decr 1
                  | k == 0 = Decr 0
                  | k < 0  = Un
abstract Un = Un
abstract (Mat m) = Mat $ absCM m

absCM :: Matrix Order -> Matrix Order
absCM = fmap abstract
-- absCM = map (map abstract) 
                  
-- the one is never needed for matrix multiplication
ordRing :: (?cutoff :: Int) => Semiring Order
ordRing = Semiring { add = maxO , mul = comp , zero = Un } -- , one = Decr 0 }

-- composition = sequence of calls
comp :: (?cutoff :: Int) => Order -> Order -> Order
comp _ Un = Un
comp Un _ = Un
comp (Decr k) (Decr l) = decr (k + l)
comp (Mat m1) (Mat m2) = if (composable m1 m2) then
                             Mat $ M.mul ordRing m1 m2
                         else
                             comp (collapse m1) (collapse m2)
comp (Decr 0) (Mat m) = Mat m
comp (Mat m) (Decr 0) = Mat m
comp o (Mat m) = comp o (collapse m)
comp (Mat m) o = comp (collapse m) o

maxO :: (?cutoff :: Int) => Order -> Order -> Order
maxO o1 o2 = case (o1,o2) of 
               (Un,_) -> o2
               (_,Un) -> o1
               (Decr k, Decr l) -> Decr (max k l) -- cutoff not needed
               (Mat m1, Mat m2) -> if (sameSize m1 m2) then
                                       Mat $ M.add maxO m1 m2
                                   else
                                       maxO (collapse m1) (collapse m2)
               (Mat m1,_) -> maxO (collapse m1) o2
               (_,Mat m2) -> maxO o1 (collapse m2)

minO :: (?cutoff :: Int) => Order -> Order -> Order
minO o1 o2 = case (o1,o2) of
               (Un,_) -> Un
               (_,Un) -> Un
               (Decr k, Decr l) -> decr (min k l)
               (Mat m1, Mat m2) -> if (sameSize m1 m2) then
                                       Mat $ minM m1 m2
                                   else
                                       minO (collapse m1) (collapse m2)
               (Mat m1,_) -> minO (collapse m1) o2
               (_,Mat m2) -> minO o1 (collapse m2)

{-
-- for non empty lists:
minimumO :: (?cutoff :: Int) => [Order] -> Order
minimumO = foldl1 minO 
-}

-- | pointwise minimum
minM :: (?cutoff :: Int) => Matrix Order -> Matrix Order -> Matrix Order
minM = M.intersectWith minO
{-
minM m1 m2 = [ minV x y | (x,y) <- zip m1 m2]
 where
   minV :: Vector Order -> Vector Order -> Vector Order
   minV v1 v2 = [ minO x y | (x,y) <- zip v1 v2]
-}

maxL :: (?cutoff :: Int) => [Order] -> Order
maxL = foldl1 maxO 

minL :: (?cutoff :: Int) => [Order] -> Order
minL = foldl1 minO 

{- collapse m

We assume that m codes a permutation:  each row has at most one column
that is not Un. 

To collapse a matrix into a single value, we take the best value of
each column and multiply them.  That means if one column is all Un,
i.e., no argument relates to that parameter, than the collapsed value
is also Un.

This makes order multiplication associative.


collapse :: (?cutoff :: Int) => Matrix Order -> Order
collapse m = foldl1 comp (map maxL (M.transpose m))

-}


{- collapse m

We assume that m codes a permutation:  each row has at most one column
that is not Un.

To collapse a matrix into a single value, we take the best value of
each column and multiply them.  That means if one column is all Un,
i.e., no argument relates to that parameter, than the collapsed value
is also Un.

This makes order multiplication associative.

-}
collapse :: (?cutoff :: Int) => Matrix Order -> Order
collapse m = case M.toLists (M.transpose m) of
--   [] -> __IMPOSSIBLE__   -- This can never happen if order matrices are generated by the smart constructor
   m' -> foldl1 comp $ map (foldl1 maxO) m'



type Vector a = [a]
type NaiveMatrix a = [Vector a]

--- 
-- matrix stuff

{-
data Semiring a = Semiring { add :: (a -> a -> a) , mul :: (a -> a -> a) , one :: a , zero :: a } 
-}

ssum :: Semiring a -> Vector a -> a
ssum sem v = foldl (add sem) (zero sem) v 

vadd :: Semiring a -> Vector a -> Vector a -> Vector a
vadd sem v1 v2 = [ (add sem) x y | (x,y) <- zip v1 v2]

scalarProdukt :: Semiring a -> Vector a -> Vector a -> a
scalarProdukt sem xs ys = ssum sem [(mul sem) x y  | (x,y) <- zip xs ys]

madd :: Semiring a -> NaiveMatrix a -> NaiveMatrix a -> NaiveMatrix a
madd sem m1 m2 = [ vadd sem x y | (x,y) <- zip m1 m2]

transp :: NaiveMatrix a -> NaiveMatrix a 
transp [] = []
transp y = [[ z!!j | z<-y] | j<-[0..s]]
    where
    s = length (head y)-1

mmul :: Show a => Semiring a -> NaiveMatrix a -> NaiveMatrix a -> NaiveMatrix a
mmul sem m1 m2 = let m = 
                         [[scalarProdukt sem r c | c <- transp m2] | r<-m1 ]
                 in m 
diag :: NaiveMatrix a -> Vector a
diag [] = []
diag m = [ (m !! j) !! j | j <- [ 0..s] ] 
   where
     s = length (head m) - 1

elems :: NaiveMatrix a -> Vector a
elems m = concat m

{-
ok :: Matrix a -> Matrix a -> Bool
ok m1 m2 = (length m1) == length m2 
-}

sameSize :: Matrix a -> Matrix a -> Bool
sameSize m1 m2 = M.size m1 == M.size m2

composable :: Matrix a -> Matrix a -> Bool
composable m1 m2 = M.rows (M.size m1) == M.cols (M.size m2)

---

-- create a call matrix
-- each row is for one argument  of the callee
-- each column for one parameter of the caller
compareArgs :: (?cutoff :: Int) => TSO Name -> [Pattern] -> [Expr] -> Arity -> Matrix Order
compareArgs tso _ [] _ = empty
compareArgs tso [] _ _ = empty
compareArgs tso pl el ar_g = 
  M.fromLists (M.Size { M.rows = fullArity ar_g , M.cols = length pl }) $
    map (\ e -> map (\ p -> --traceTerm ("comparing " ++ show e ++ " to " ++ show p) $ 
                                    compareExpr tso e p) pl) el
{-
compareArgs tso pl el ar_g = 
        let 
            diff = ar_g - length el 
            fill = if diff > 0 then
                       replicate diff (replicate (length pl) Un)
                   else []
            cmp = map (\ e -> (map (\ p -> --traceTerm ("comparing " ++ show e ++ " to " ++ show p) $ 
                                    compareExpr tso e p) pl)) el
        in
          cmp ++ fill
-}
            
{-             
compareExpr :: (?cutoff :: Int) => Expr -> Pattern -> Order
compareExpr e p =  
   case (e,p) of 
      (_,UnusableP _) -> Un  
      (_,DotP e') -> case exprToPattern e' of
                       Nothing -> if e == e' then Decr 0 else Un
                       Just p' -> compareExpr e p'
      (Var i,p) -> traceTerm ("compareVar " ++ show i ++ " " ++ show p) $ compareVar i p 
      (App (Var i) _,p) -> compareVar i p 
      (Con _ n1,ConP _ n2 [])  | n1 == n2 -> Decr 0
      (App (Con _ n1) [e1],ConP _ n2 [p1]) | n1 == n2 -> compareExpr e1 p1 
      (App (Con _ n1) args,ConP _ n2 pl) | n1 == n2 && length args == length pl -> 
              Mat (map (\ e -> (map (compareExpr e) pl)) args)
              -- without extended order :  minL $ zipWith compareExpr args pl
      (Succ e2,SuccP p2) -> compareExpr e2 p2     
      -- new cases for counting constructors
      (Succ e2,p) -> Decr (-1) `comp` compareExpr e2 p
      (App (Con _ n1) args@(_:_), p) -> Decr (-1) `comp` minL (map (\e -> compareExpr e p) args)
      _ -> Un
-}


             
compareExpr :: (?cutoff :: Int) => TSO Name -> Expr -> Pattern -> Order
compareExpr tso e p =  
  let ret o = traceTerm ("comparing expression " ++ show e ++ " to pattern " ++ show p ++ " returns " ++ show o) o in
    ret $ compareExpr' tso e p

compareExpr' :: (?cutoff :: Int) => TSO Name -> Expr -> Pattern -> Order
compareExpr' tso (Ann e) p = compareExpr' tso (unTag e) p  
compareExpr' tso e p =  
   case (spineView e, p) of 
      (_,UnusableP _) -> Un
--      (Erased e,_)    -> compareExpr' tso e p  
      (_,ErasedP p)   -> compareExpr' tso e p  
      (_,DotP e') -> case exprToPattern e' of
                       Nothing ->  if e == e' then Decr 0 else Un
                       Just p' -> compareExpr' tso e p'
      ((Var i,_), p) -> -- traceTerm ("compareVar " ++ show i ++ " " ++ show p) $ 
                         compareVar tso i p 
--      (Con _ n1,ConP _ n2 [])  | n1 == n2 -> Decr 0
--      (App (Con _ n1) [e1],ConP _ n2 [p1]) | n1 == n2 -> compareExpr' tso e1 p1 
      ((Def (DefId (ConK _) n1),args),ConP _ n2 pl) | n1 == n2 && length args == length pl -> 
          let os = zipWith (compareExpr' tso) args pl 
          in  trace ("compareExpr (con/con case): os = " ++ show os) $
              if null os then Decr 0 else minL os
{- 2011-12-16 deactivate structured (matrix) orders
          orderMat $ 
            M.fromLists (M.Size { M.rows = length args, M.cols = length pl }) $
               map (\ e -> map (compareExpr' tso e) pl) args
              -- without extended order :  minL $ zipWith compareExpr' tso args pl
-}
      ((Succ e2,_),SuccP p2) ->  compareExpr' tso e2 p2     
      -- new cases for counting constructors
      ((Succ e2,_),p) ->  Decr (-1) `comp` compareExpr' tso e2 p
      ((Def (DefId (ConK _) n1),args@(_:_)), p) ->  Decr (-1) `comp` minL (map (\e -> compareExpr' tso e p) args)
      ((Proj n1,[]), ProjP n2) | n1 == n2 -> Decr 0
      _ -> Un

compareVar :: (?cutoff :: Int) => TSO Name -> Name -> Pattern -> Order
compareVar tso n p = 
  let ret o = o in -- traceTerm ("comparing variable " ++ n ++ " to " ++ show p ++ " returns " ++ show o) o in
    case p of
      UnusableP _ -> ret Un
      ErasedP p   -> compareVar tso n p
      VarP n2 -> if n == n2 then Decr 0 else  
        case TSO.diff n n2 tso of -- if n2 is the k-th father of n, then it is a decrease by k
          Nothing -> ret Un
          Just k -> ret $ decr k
      SizeP n1 n2 -> if n == n2 then Decr 0 else  
        case TSO.diff n n2 tso of -- if n2 is the k-th father of n, then it is a decrease by k
          Nothing -> ret Un
          Just k -> ret $ decr k
      ConP pi c (p:pl) | coPat pi == Ind -> 
        comp (Decr 1) (maxL (map (compareVar tso n) (p:pl)))
      ConP{}   -> ret Un
      ProjP{}   -> ret Un
      SuccP p2 -> comp (Decr 1) (compareVar tso n p2)
      DotP e -> case (exprToPattern e) of
                    Nothing -> ret $ Un
                    Just p' -> compareVar tso n p'
      _ -> error $ "NYI: compareVar " ++ show n ++ " to " ++ show p -- ret $ Un
      
{- REIMPLEMENTED in Abstract.hs
exprToPattern :: Expr -> Maybe Pattern
exprToPattern e = 
    case e of
      Var n -> Just $ VarP n
      (Succ e) -> case exprToPattern e of
                    Nothing -> Nothing
                    Just p -> Just $ SuccP p
      (App (Con co n) el) -> case exprsToPatterns el of
                               Nothing -> Nothing
                               Just pl -> Just $ ConP co n pl
      (Con co n) -> Just $ ConP co n []
      _ -> Nothing

exprsToPatterns :: [Expr] -> Maybe [Pattern]
exprsToPatterns [] = Just []
exprsToPatterns (e:el) = case exprToPattern e of
                           Nothing -> Nothing
                           Just p -> case exprsToPatterns el of
                                       Nothing -> Nothing
                                       Just pl -> Just (p:pl)
-}

---

type Index = Name

data Call = Call { source :: Index , target :: Index , matrix :: CallMatrix }  
            deriving (Eq,Show,Ord)

-- call matrix:
-- each row is for one argument  of the callee (target)
-- each column for one parameter of the caller (source)

type CallMatrix = Matrix Order

-- for two matrices m m' of the same dimensions,
-- m `subsumes` m'  if  pointwise the entries of m are smaller than of m' 
subsumes :: Matrix Order -> Matrix Order -> Bool
subsumes m m' = M.all (uncurry leq) mm'
  where mm' = M.zip m m' -- create one matrix of pairs
{-
subsumes m m' = all (all (uncurry leq)) mm'
  where mm' = zipWith zip m m' -- create one matrix of pairs
-}

-- Order forms itself a partial order
leq :: Order -> Order -> Bool
leq Un _ = True
leq (Decr k) (Decr l) = k <= l
leq (Mat m) (Mat m') = subsumes m m'
leq _ _ = False

-- for two matrices m m' such that m `subsumes` m' 
-- m `progress` m'  any positive entry in m' is smaller in m 
progress :: Matrix Order -> Matrix Order -> Bool
progress m m' = M.any (uncurry decrToward0) mm'
  where mm' = M.zip m m' -- create one matrix of pairs
{-
progress m m' = any (any (uncurry decrToward0)) mm'
  where mm' = zipWith zip m m' -- create one matrix of pairs
-}

decrToward0 :: Order -> Order -> Bool
decrToward0 Un (Decr l) = True && l >= 0 
decrToward0 (Decr k) (Decr l) = k < l  && l >= 0
decrToward0 (Mat m) (Mat m') = progress m m'
decrToward0 _ _ = False


{- call pathes 
 
  are lists of names of length >=2

  [f,g,h] = f --> g --> h
-}

newtype CallPath = CallPath { getCallPath :: [Name] } deriving Eq

instance Show CallPath where
  show (CallPath [g]) = show g
  show (CallPath (f:l)) = show f ++ "-->" ++ show (CallPath l)

emptyCP :: CallPath
emptyCP = CallPath []

mkCP :: Name -> Name -> CallPath
mkCP src tgt = CallPath [src, tgt]

mulCP :: CallPath -> CallPath -> CallPath
mulCP cp1@(CallPath one) cp2@(CallPath (g:two)) = 
  if last one == g then CallPath (one ++ two) 
  else error ("internal error: Termination.mulCP: trying to compose callpath " ++ show cp1 ++ " with " ++ show cp2)

compatibleCP :: CallPath -> CallPath -> Bool
compatibleCP (CallPath one) (CallPath two) = head one == head two && last one == last two

{-
addCP :: CallPath -> CallPath -> CallPath
addCP (CallPath []) cp = cp
addCP cp (CallPath []) = cp
addCP cp1 cp2 = if cp1 == cp2 then cp1 else error ("internal error: Termination.addCP: trying to blend non-equal callpathes " ++ show cp1 ++ " and " ++ show cp2)

cpRing :: Semiring CallPath
cpRing = Semiring { add = addCP , mul = mulCP , one = undefined , zero = emptyCP }
-}

-- composed calls

type CompCall = (CallPath, CallMatrix)

mulCC :: (?cutoff :: Int) => CompCall -> CompCall -> CompCall
mulCC cc1@(cp1, m1) cc2@(cp2, m2) = zipPair mulCP (flip (M.mul ordRing)) cc1 cc2

subsumesCC :: CompCall -> CompCall -> Bool
subsumesCC cc1@(cp1, m1) cc2@(cp2, m2) = 
  if compatibleCP cp1 cp2 then m1 `subsumes` m2
   else error ("internal error: Termination.subsumesCC: trying to compare composed call " ++ show cc2 ++ " with " ++ show cc1)

progressCC :: CompCall -> CompCall -> Bool
progressCC cc1@(cp1, m1) cc2@(cp2, m2) = progress m1 m2


{- call graph completion

organize call graph as a square matrix
 
  Name * Name -> Set CallMatrix

the completion process finds new calls by composing old calls.  
There are two qualities of new calls.

  1) a completely new call or a call matrix in which one cell
     progressed from (Decr k | k > 0) towards -infty, i.e. a positive
     entry got smaller

  2) a negative entry got smaller

As long as 1-calls are found, continue completion.  
[ I think 2-calls can be ignored when deciding whether to cont. ]

 -}                        

-- sets of call matrices

type CMSet    = [CompCall]  -- normal form: no CM subsumes another

cmRing :: (?cutoff :: Int) => Semiring CMSet
cmRing = Semiring { add = unionCMSet , mul = mulCMSet , zero = [] } -- one = undefined , 

type Progress = Writer Any
type ProgressH = Writer (Any, Any)

firstHalf = (Any True, Any False)
secondHalf = (Any False, Any True)

-- fullProgress = Sum 2
-- halfProgress = Sum 1

-- we keep CMSets always in normal form
-- progress reported if m is "better" than one of ms
-- progress can only be reported if m is being added, i.e., not subsumed
addCMh :: CompCall -> CMSet -> ProgressH CMSet
addCMh m [] = traceProg ("adding new call " ++ show m) $ do
  tell firstHalf
  return $ [m]
addCMh m (m':ms) = 
  if m' `subsumesCC` m then traceTerm ("discarding new call " ++ show m) $
     return $ m':ms -- terminate early
   else do (ms', (Any h1, Any h2)) <- listen $ addCMh m ms
           when (h1 && not h2 && m `progressCC` m') $ do
             traceProgM ("progress made by " ++ show m ++ " over " ++ show m')
             tell secondHalf -- $ Any True
           if m `subsumesCC` m' then traceTerm ("discarding old call " ++ show m') $ 
                 return ms'
            else return $ m' : ms'

addCM' :: CompCall -> CMSet -> Progress CMSet
addCM' m ms = mapWriter (\(ms, (Any h1, Any h2)) -> (ms, Any $ h1 && h2)) (addCMh m ms)

-- progress is reported if one of ms is "better" than ms'
-- or if the oldset was empty and is no longer
-- unionCMSet' addition oldset
unionCMSet' :: CMSet -> CMSet -> Progress CMSet
unionCMSet' [] []  = return []
unionCMSet' ms []  = tell (Any True) >> return ms
unionCMSet' ms ms' = foldM (flip addCM') ms' ms

-- non-monadic versions
addCM :: CompCall -> CMSet -> CMSet
addCM m ms = fst $ runWriter (addCM' m ms)

unionCMSet :: CMSet -> CMSet -> CMSet
unionCMSet ms ms' = fst $ runWriter (unionCMSet' ms ms')

mulCMSet :: (?cutoff :: Int) => CMSet -> CMSet -> CMSet
mulCMSet ms ms' = foldl (flip addCM) [] $ [ mulCC m m' | m <- ms, m' <- ms' ]

{- call graph entries

type CGEntry = (CallPath, CMSet)

cgeRing :: Semiring CGEntry
cgeRing = Semiring { add = zipPair addCP unionCMSet, 
                     mul = zipPair mulCP mulCMSet, 
                     one = undefined, 
                     zero = (emptyCP, []) }

addCGEntry' :: CGEntry -> CGEntry -> Progress CGEntry
addCGEntry' (cp1, ms1) (cp2, ms2) = do
  let cp = addCP cp1 cp2
  traceTermM ("call")
  ms <- unionCMSet' ms1 ms2
  return $ (cp, ms)
-}

-- call graphs

type CallGraph = NaiveMatrix CMSet -- CGEntry

stepCG :: (?cutoff :: Int) => CallGraph -> Progress CallGraph
stepCG cg = do
  traceProgM ("next iteration")
  traceProgM ("old cg " ++ show cg)
  traceProgM ("composed calls " ++ show cg') 
  traceProgM ("adding new calls to callgraph...")
  zipWithM (zipWithM unionCMSet') cg' cg
  where cg' = mmul cmRing cg cg

{- "each idempotent call f->f has a decreasing arg" is an invariant
   of good call graphs.  Thus, we can stop call graph completion
   as soon as we see it violated.

   "idempotent" is defined on abstracted call matrices, i.e.,
   those that only have <, <=, ? and are not counting.
 -}
complCGraph :: (?cutoff :: Int) => CallGraph -> CallGraph
complCGraph cg = 
  let (cg', Any prog) = runWriter $ stepCG cg
  in  if prog && checkAll cg' then complCGraph cg' else cg'

checkAll :: (?cutoff :: Int) => CallGraph -> Bool
checkAll cg = all (all (checkIdem . snd)) $ diag cg

-- each idempotent call needs a decreasing diagonal entry
checkIdem :: (?cutoff :: Int) => CallMatrix -> Bool
checkIdem cm =
  let cm'   = M.mul ordRing cm cm
      eqAbs = (absCM cm) == (absCM cm') 
      d     = M.diagonal cm
  in  traceTerm ("checkIdem: cm = " ++ show cm ++ " cm' = " ++ show cm ++ " eqAbs = " ++ show eqAbs ++ " d = " ++ show d) $ 
      -- if cm `subsumes` cm'
      if eqAbs
       then any isDecr d else True

{- generate a call graph from a list of names and list of calls
1. group calls by source, obtaining a list of row
-}

{- THIS IS WRONG:
makeCG :: [Name] -> [Call] -> CallGraph
makeCG names calls = map (\ tgt -> mkRow tgt [ c | c <- calls, target c == tgt ]) names
  where mkRow tgt calls = map (\ src ->  unionCMSet [ (mkCP src tgt, matrix c) | c <- calls, source c == src ] []) names 
-}

makeCG :: [Name] -> [Call] -> CallGraph
makeCG names calls = map (\ src -> mkRow src [ c | c <- calls, source c == src ]) names
  where mkRow src calls = map (\ tgt ->  unionCMSet [ (mkCP src tgt, matrix c) | c <- calls, target c == tgt ] []) names 

{-
callComb :: Call -> Call -> Call
callComb (Call s1 t1 m1) (Call s2 t2 m2) = Call s2 t1 (mmul ordRing m1 m2)

cgComb :: [Call] -> [Call] -> [Call]
cgComb cg1 cg2 = [ callComb c1 c2 | c1 <- cg1 , c2 <- cg2 , (source c1 == target c2)]

complete :: [Call] -> [Call] 
complete cg = traceTerm ("call graph: " ++ show cg) $
  let cg' = complete' cg -- $ Set.fromList cg 
  in -- traceTerm ("complete " ++ show cg') 
       cg' -- Set.toList cg'

complete' :: [Call] -> [Call]  -- Set Call -> Set Call
complete' cg =
              let cgs = Set.fromList cg
                  cgs' = Set.union cgs (Set.fromList $ cgComb cg cg ) 
                  cg' = Set.toList cgs'
              in
                if (cgs == cgs') then cg else complete' cg'

checkAll :: [Call] -> Bool
checkAll x = all checkIdem x

-- each idempotent call needs a decreasing diagonal entry
checkIdem :: Call -> Bool
checkIdem c = let cc = callComb c c
                  d = diag (matrix cc)
                  containsDecr = any isDecr d
              in (not (c == cc)) || containsDecr
-}
isDecr :: Order -> Bool
isDecr o = case o of
             (Decr k) -> k > 0
             (Mat m) -> any isDecr (M.diagonal m)
             _ -> False

    
-------------------

-- top level function
terminationCheck :: MonadAssert m => [Fun] -> m ()
terminationCheck funs = do
       let ?cutoff = cutoff
       traceTermM $ "terminationCheck " ++ show funs
       let tl = terminationCheckFuns funs
       let nl = map fst tl
       let bl = map snd tl
       let nl2 = [ n | (n,b) <- tl , b == False ]
       case (and bl) of
            True -> return ()
            False -> case nl of
                    [f] -> recoverFail ("Termination check for function " ++ show f ++ " fails ") 
                    _   -> recoverFail ("Termination check for mutual block " ++ show nl ++ " fails for " ++ show nl2) 
                                   

terminationCheckFuns :: (?cutoff :: Int) => [Fun] -> [(Name,Bool)]
terminationCheckFuns funs =
   let namar = map (\ (Fun (TypeSig n _) _ ar _) -> (n, ar)) funs
               -- collectNames funs
       names = map fst namar
       cg0 = collectCGFunDecl namar funs
   in sizeChangeTermination names cg0  

sizeChangeTermination :: (?cutoff :: Int) => [Name] -> [Call] -> [(Name,Bool)]
sizeChangeTermination names cg0 =
   let cg1 = makeCG names cg0
       cg = complCGraph $ cg1
       beh = zip names $ map (all (checkIdem . snd)) $ diag cg 
   in traceTerm ("collected names: " ++ show names) $
      traceTerm ("call graph: " ++ show cg0) $
      traceTerm ("normalized call graph: " ++ show cg1) $
      traceTerm ("completed call graph: " ++ show cg) $
      traceTerm ("recursion behaviours" ++ show beh) $
      beh  


{-
terminationCheckFuns :: [ (TypeSig,[Clause]) ] -> [(Name,Bool)]
terminationCheckFuns funs =
    let beh = recBehaviours funs 
    in
      traceTerm ("recursion behaviours" ++ show beh) $
        zip (map fst beh) (map (checkAll . snd ) beh )

-- This is the main driver.         
recBehaviours :: [ (TypeSig,[Clause]) ] -> [(Name,[Call])]
recBehaviours funs = let names = map fst $ collectNames funs
                         cg0 = collectCGFunDecl funs
                         cg = complete cg0
                     in traceTerm ("collected names: " ++ show names) $
                        traceTerm ("call graph: " ++ show cg0) $
                        groupCalls names [ c | c <- cg , (target c == source c) ]


groupCalls :: [Name] -> [Call] -> [(Name,[Call])]
groupCalls [] _ = []
groupCalls (n:nl) cl = (n, [ c | c <- cl , (source c == n) ]) : groupCalls nl cl
-}

{-
ccFunDecl :: [ ( TypeSig,[Clause]) ] -> [Call]
ccFunDecl funs = complete $ collectCGFunDecl funs
-}

collectCGFunDecl :: (?cutoff :: Int) => [(Name,Arity)] -> [Fun] -> [Call]
collectCGFunDecl names funs =
      concatMap (collectClauses names) funs
          where
            collectClauses :: [(Name,Arity)] -> Fun -> [Call]
            collectClauses names (Fun (TypeSig n _) _ ar cll) = collectClause names n cll
            collectClause :: [(Name,Arity)] -> Name -> [Clause] -> [Call]
            collectClause names n ((Clause _ pl Nothing):rest) = collectClause names n rest
            collectClause names n ((Clause _ pl (Just rhs)):rest) =
              traceTerm ("collecting calls in " ++ show rhs) $ 
                (collectCallsExpr names n pl rhs) ++ (collectClause names n rest) 
            collectClause names n [] = []

{- RETIRED
arity :: [Clause] -> Int
arity [] = 0
arity (Clause pl e:l) = length pl
-}

{- RETIRED (map)
collectNames :: [Fun] -> [(Name,Arity)]
collectNames [] = []                
collectNames (Fun (TypeSig n _) ar cls : rest) = (n,ar) : (collectNames rest)
-}

-- | harvest i > j  from  case i { $ j -> ...} 
tsoCase :: TSO Name -> Expr -> [Clause] -> TSO Name
tsoCase tso (Var x) [Clause _ [SuccP (VarP y)] _] = TSO.insert y (1,x) tso
tsoCase tso _ _ = tso

collectCallsExpr :: (?cutoff :: Int) => [(Name,Arity)] -> Name -> [Pattern] -> Expr -> [Call]
collectCallsExpr nl f pl e = traceTerm ("collectCallsExpr " ++ show e) $
  loop tso e where
    tso = tsoFromPatterns pl
    loop tso (Ann e) = loop tso (unTag e)
    loop tso e = headcalls ++ argcalls where
      (hd, args) = spineView e -- $ ignoreTopErasure e  
      argcalls = concatMap (loop tso) args
      headcalls = case hd of
          (Def (DefId FunK g)) -> 
              case lookup g nl of
                Nothing -> []
                Just ar_g -> 
                  traceTerm ("found call from " ++ show f ++ " to " ++ show g) $ 
                             let (Just ar_f) = lookup f nl
                                 (Just f') = List.elemIndex (f,ar_f) nl
                                 (Just g') = List.elemIndex (g,ar_g) nl
                                 m = compareArgs tso pl args ar_g
                                 cg = Call { source = f
                                           , target = g
                                           , matrix = m }
                             in 
                               traceTerm ("found call " ++ show cg) $ 
                                 [cg]
          (Case e cls) -> loop tso e ++ concatMap (loop (tsoCase tso e cls)) (map (maybe Irr id . clExpr) cls)
          (Lam _ _ e1) -> loop tso e1
          (LLet tb e1 e2) ->  
             (loop tso e1) ++ -- type won't get evaluated 
             (loop tso e2) 
          (Quant _ (TBind x dom) e2) -> (loop tso (typ dom)) ++ (loop tso e2)
          (Quant _ (TMeasure mu) e2) -> Foldable.foldMap (loop tso) mu ++ (loop tso e2)
          (Quant _ (TBound beta) e2) -> Foldable.foldMap (loop tso) beta ++ (loop tso e2)
          (Sing e1 e2) -> (loop tso e1) ++ (loop tso e2)
          (Pair e1 e2) -> (loop tso e1) ++ (loop tso e2)
          (Succ e) -> loop tso e
          (Max es) -> concatMap (loop tso) es
          (Plus es) -> concatMap (loop tso) es
          Sort (SortC{})  -> []
          Sort (Set e)    -> loop tso e
          Sort (CoSet e)  -> loop tso e
          Var{}   -> []
          Zero{} -> []
          Infty{} -> []
          Def{}   -> []
          Irr{}   -> []
          Proj{}   -> []
          Record rs -> Foldable.foldMap (loop tso . snd) rs
          Ann e1 -> loop tso (unTag e1)
--          Con{}   -> []
--          Let{}   -> []
          Meta{}  -> error $ "collect calls in unresolved meta variable " ++ show e
          _ -> error $ "NYI: collect calls in " ++ show e

{-
collectCallsExpr :: (?cutoff :: Int) => [(Name,Int)] -> Name -> [Pattern] -> Expr -> [Call]
collectCallsExpr nl f pl e = 
  traceTerm ("collectCallsExpr " ++ show e) $
    case e of
      (App (Def g) args) -> 
        let calls = concatMap (collectCallsExpr nl f pl) args
            gIn = lookup g nl 
        in
         traceTerm ("found call from " ++ f ++ " to " ++ g) $
          case gIn of
            Nothing -> calls
            Just ar_g -> let (Just ar_f) = lookup f nl
                             (Just f') = List.elemIndex (f,ar_f) nl
                             (Just g') = List.elemIndex (g,ar_g) nl
                             m = compareArgs pl args ar_g
                             cg = Call { source = f
                                       , target = g
                                       , matrix = m }
                         in 
                           traceTerm ("found call " ++ show cg) $ 
                             cg:calls
      (Def g) ->  collectCallsExpr nl f pl (App (Def g) []) 
      (App e args) -> concatMap (collectCallsExpr nl f pl) (e:args)
      (Case e cls) -> concatMap (collectCallsExpr nl f pl) (e:map clExpr cls)
      (Lam _ _ e1) -> collectCallsExpr nl f pl e1
      (LLet _ e1 t1 e2) ->  (collectCallsExpr nl f pl e1) ++ -- type won't get evaluated 
                            (collectCallsExpr nl f pl e2) 
      (Pi _ _ e1 e2) -> (collectCallsExpr nl f pl e1) ++
                              (collectCallsExpr nl f pl e2)
      (Sing e1 e2) -> (collectCallsExpr nl f pl e1) ++
                              (collectCallsExpr nl f pl e2)
      (Succ e1) -> collectCallsExpr nl f pl e1
      Sort{}  -> []
      Var{}   -> []
      Infty{} -> []
      Con{}   -> []
      Let{}   -> []
      Meta{}  -> error $ "collect calls in unresolved meta variable " ++ show e
      _ -> error $ "NYI: collect calls in " ++ show e
-}

----------------------------------------------------------------------
{- Foetus II - Counting Lexicographic Termination (delta-Foetus)

delta-SCT [Ben-Amram 2006] is too inefficient, at least with the bound
given in the paper.

  B(G) = (k + 1)2^k · m^2 · 2^(2k+1) (m∆)^(3k+1) (k + 1)^(3k^2+3k+1)

is an upper bound on the length of the longest path to be looked at to
exclude non-termination.

I guess that both argument permutation and counting is not very
common.  So an approach would be

- try to show termination with SCT
- try to show termination with delta-Foetus

Call graph completion in delta-Foetus

1. Iterate as long new simple cycles show up (i.e. cycles with no subcycles)

2. Find the possible lexicographic termination orders to for each function

3. Continue iterating while any of the arguments involved in any of the termination orders gets worse.  Some termination order hypotheses might collapse.

4. Stop when all hypotheses have collapsed (FAIL) or when no standing hypotheses gets any worse (SUCCESS).

Implementation:

After 1. save for each function and each of its arguments the worst
recursive behavior in any of the calls.  This map will be used to
monitor progress.


Careful:

  f x = f (x-1) | g (x - 100)
  g x = g (x+1) | f (x - 100)

Bad call f->f only found after 201 iterations of g!

Idea:  regular expressions over call matrices!

  (m1 + m2^*)^*

-}

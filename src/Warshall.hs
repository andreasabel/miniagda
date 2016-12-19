{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Warshall where

{- construct a graph from constraints

   x + n <= y   becomes   x ---(-n)---> y
   x <= n + y   becomes   x ---(+n)---> y

the default edge (= no edge is) labelled with infinity

building the graph involves keeping track of the node names.
We do this in a finite map, assigning consecutive numbers to nodes.
-}


import Control.Monad.State
import Data.Maybe -- fromJust
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Debug.Trace
import Util


traceSolve msg a = a -- trace msg a 
traceSolveM msg = return () -- traceM msg
{-
traceSolve msg a = trace msg a 
traceSolveM msg = traceM msg
-}


-- semi rings ----------------------------------------------------

class SemiRing a where
  oplus  :: a -> a -> a
  otimes :: a -> a -> a
  ozero  :: a -- neutral for oplus, dominant for otimes
  oone   :: a -- neutral for otimes

type Matrix a = Array (Int,Int) a

-- assuming a square matrix
warshall :: SemiRing a => Matrix a -> Matrix a
warshall a0 = loop r a0 where 
  b@((r,c),(r',c')) = bounds a0 -- assuming r == c and r' == c'
  loop k a | k <= r' = 
    loop (k+1) (array b [ ((i,j), 
                           (a!(i,j)) `oplus` ((a!(i,k)) `otimes` (a!(k,j))))
                        | i <- [r..r'], j <- [c..c'] ])
           | otherwise = a

-- edge weight in the graph, forming a semi ring 

data Weight = Finite Int | Infinite 
              deriving (Eq)

inc :: Weight -> Int -> Weight
inc Infinite   n = Infinite
inc (Finite k) n = Finite (k + n)

instance Show Weight where
  show (Finite i) = show i
  show Infinite   = "."

instance Ord Weight where
  a <= Infinite = True
  Infinite <= b = False
  Finite a <= Finite b = a <= b

instance SemiRing Weight where
  oplus = min

  otimes Infinite _ = Infinite
  otimes _ Infinite = Infinite
  otimes (Finite a) (Finite b) = Finite (a + b)

  ozero = Infinite
  oone  = Finite 0
 
-- constraints ---------------------------------------------------

-- nodes of the graph are either 
-- * flexible variables (with identifiers drawn from Int), 
-- * rigid variables (also identified by Ints), or 
-- * constants (like 0, infinity, or anything between)

data Node rigid
  = Rigid rigid
  | Flex  FlexId
    deriving (Eq, Ord)

instance Show rigid => Show (Node rigid) where
  show (Flex  i) = "?" ++ show i
  show (Rigid r) = show r

data Rigid = RConst Weight
           | RVar RigidId
             deriving (Eq, Ord)

instance Show Rigid where
  show (RVar i) = "v" ++ show i
  show (RConst Infinite)   = "#"
  show (RConst (Finite n)) = show n

type NodeId  = Int
type RigidId = Int
type FlexId  = Int
type Scope   = RigidId -> Bool  
-- which rigid variables a flex may be instatiated to

infinite (RConst Infinite) = True
infinite _ = False

-- isBelow r w r'  
-- checks, if r and r' are connected by w (meaning w not infinite)
-- wether r + w <= r'
-- precondition: not the same rigid variable
isBelow :: Rigid -> Weight -> Rigid -> Bool
isBelow _ Infinite _ = True
isBelow _ n (RConst Infinite) = True
-- isBelow (RConst Infinite)   n (RConst (Finite _)) = False
isBelow (RConst (Finite i)) (Finite n) (RConst (Finite j)) = i + n <= j
isBelow _ _ _ = False -- rigid variables are not related

-- a constraint is an edge in the graph
data Constrnt edgeLabel rigid flexScope
  = NewFlex FlexId flexScope
  | Arc (Node rigid) edgeLabel (Node rigid)
-- Arc v1 k v2  at least one of v1,v2 is a VMeta (Flex), 
--              the other a VMeta or a VGen (Rigid)
-- if k <= 0 this means  $^(-k) v1 <= v2
-- otherwise                    v1 <= $^k v3

type Constraint = Constrnt Weight Rigid Scope

arc :: Node Rigid -> Int -> Node Rigid -> Constraint
arc a k b = Arc a (Finite k) b

instance Show Constraint where
  show (NewFlex i s) = "SizeMeta(?" ++ show i ++ ")"
  show (Arc v1 (Finite k) v2) 
    | k == 0 = show v1 ++ "<=" ++ show v2
    | k < 0  = show v1 ++ "+" ++ show (-k) ++ "<=" ++ show v2
    | otherwise  = show v1 ++ "<=" ++ show v2 ++ "+" ++ show k

type Constraints = [Constraint]

emptyConstraints = []

-- graph (matrix) ------------------------------------------------

data Graph edgeLabel rigid flexScope = Graph 
  { flexScope :: Map FlexId flexScope       -- scope for each flexible var
  , nodeMap :: Map (Node rigid) NodeId      -- node labels to node numbers
  , intMap  :: Map NodeId (Node rigid)      -- node numbers to node labels
  , nextNode :: NodeId                      -- number of nodes (n)
  , graph :: NodeId -> NodeId -> edgeLabel  -- the edges (restrict to [0..n[)
  }

-- the empty graph: no nodes, edges are all undefined (infinity weight)
initGraph :: SemiRing edgeLabel => Graph edgeLabel rigid flexScope
initGraph = Graph Map.empty Map.empty Map.empty 0 (\ x y -> ozero)

-- the Graph Monad, for constructing a graph iteratively
type GM edgeLabel rigid flexScope = State (Graph edgeLabel rigid flexScope)

addFlex :: FlexId -> flexScope -> GM edgeLabel rigid flexScope ()
addFlex x scope = do
  st <- get
  put $ st { flexScope = Map.insert x scope (flexScope st) }


-- i <- addNode n  returns number of node n. if not present, it is added first
addNode :: (Eq rigid, Ord rigid) => (Node rigid) -> GM edgeLabel rigid flexScope Int
addNode n = do
  st <- get
  case Map.lookup n (nodeMap st) of
    Just i -> return i
    Nothing -> do let i = nextNode st
                  put $ st { nodeMap = Map.insert n i (nodeMap st)
                           , intMap = Map.insert i n (intMap st)
                           , nextNode = i + 1
                           }
                  return i

-- addEdge n1 k n2  
-- improves the weight of egde n1->n2 to be at most k
-- also adds nodes if not yet present
addEdge :: (Eq rigid, Ord rigid, SemiRing edgeLabel) => (Node rigid) -> edgeLabel -> (Node rigid) -> GM edgeLabel rigid flexScope ()
addEdge n1 k n2 = do
  i1 <- addNode n1
  i2 <- addNode n2
  st <- get
  let graph' x y = if (x,y) == (i1,i2) then k `oplus` (graph st) x y
                   else graph st x y
  put $ st { graph = graph' }

addConstraint :: (Eq rigid, Ord rigid, SemiRing edgeLabel) =>  
  Constrnt edgeLabel rigid flexScope -> GM edgeLabel rigid flexScope ()
addConstraint (NewFlex x scope) = do
  addFlex x scope
  addEdge (Flex x) oone (Flex x) -- add dummy edge to make sure each meta variable
                              -- is in the matrix and gets solved
addConstraint (Arc n1 k n2)     = addEdge n1 k n2

buildGraph :: (Eq rigid, Ord rigid, SemiRing edgeLabel) =>  
  [Constrnt edgeLabel rigid flexScope] -> Graph edgeLabel rigid flexScope
buildGraph cs = execState (mapM_ addConstraint cs) initGraph

mkMatrix :: Int -> (Int -> Int -> a) -> Matrix a
mkMatrix n g = array ((0,0),(n-1,n-1)) 
                 [ ((i,j), g i j) | i <- [0..n-1], j <- [0..n-1]]

-- displaying matrices with row and column labels --------------------

-- a matrix with row descriptions in b and column descriptions in c
data LegendMatrix a b c = LegendMatrix 
  { matrix   :: Matrix a
  , rowdescr :: Int -> b
  , coldescr :: Int -> c
  }

instance (Show a, Show b, Show c) => Show (LegendMatrix a b c) where
  show (LegendMatrix m rd cd) =
    -- first show column description
    let ((r,c),(r',c')) = bounds m
    in foldr (\ j s -> "\t" ++ show (cd j) ++ s) "" [c .. c'] ++ 
    -- then output rows
       foldr (\ i s -> "\n" ++ show (rd i) ++
                foldr (\ j t -> "\t" ++ show (m!(i,j)) ++ t) 
                      (s) 
                      [c .. c'])
             "" [r .. r'] 

-- solving the constraints -------------------------------------------

-- a solution assigns to each flexible variable a size expression
-- which is either a constant or a v + n for a rigid variable v
type Solution = Map Int MaxExpr

emptySolution :: Solution
emptySolution = Map.empty

extendSolution :: Solution -> Int -> SizeExpr -> Solution
extendSolution subst k v = Map.insertWith (++) k [v] subst

type MaxExpr = [SizeExpr]
-- newtype MaxExpr = MaxExpr { sizeExprs :: [SizeExpr] } deriving (Show)

data SizeExpr = SizeVar Int Int   -- e.g. x + 5
              | SizeConst Weight  -- a number or infinity

instance Show SizeExpr where
  show (SizeVar n 0) = show (Rigid (RVar n))
  show (SizeVar n k) = show (Rigid (RVar n)) ++ "+" ++ show k
  show (SizeConst (Finite i)) = show i
  show (SizeConst Infinite)   = "#"

-- sizeRigid r n  returns the size expression corresponding to r + n
sizeRigid :: Rigid -> Int -> SizeExpr
sizeRigid (RConst k) n = SizeConst (inc k n)
sizeRigid (RVar i)   n = SizeVar i n 

{-
apply :: SizeExpr -> Solution -> SizeExpr
apply e@(SizeExpr (Rigid _) _) phi = e
apply e@(SizeExpr (Flex  x) i) phi = case Map.lookup x phi of
  Nothing -> e
  Just (SizeExpr v j) -> SizeExpr v (i + j) 
 
after :: Solution -> Solution -> Solution
after psi phi = Map.map (\ e -> e `apply` phi) psi
-}

{-
solve :: Constraints -> Maybe Solution
solve cs = if any (\ x -> x < Finite 0) d then Nothing
     else Map.
   where gr = buildGraph cs
         n  = nextNode gr
         m  = mkMatrix n (graph gr)
         m' = warshall m
         d  = [ m!(i,i) | i <- [0 .. (n-1)] ]
         ns = keys (nodeMap gr)
-}

{- compute solution

a solution CANNOT exist if

  v < v  for a rigid variable v

  v <= v' for rigid variables v,v'

  x < v   for a flexible variable x and a rigid variable v

thus, for each flexible x, only one of the following cases is possible

  r+n <= x+m <= infty  for a unique rigid r  (meaning r --(m-n)--> x)
  x <= r+n             for a unique rigid r  (meaning x --(n)--> r)

we are looking for the least values for flexible variables that solve
the constraints.  Algorithm

while flexible variables and rigid rows left
  find a rigid variable row i
    for all flexible columns j
      if i --n--> j with n<=0 (meaning i+n <= j) then j = i + n

while flexible variables j left
  search the row j for entry i
    if j --n--> i with n >= 0 (meaning j <= i + n) then j = i 


-}

solve :: Constraints -> Maybe Solution
solve cs = traceSolve (show lm0) $ traceSolve (show lm) $ traceSolve (show cs) $
     let solution = if solvable then loop1 rigids emptySolution
                    else Nothing
     in traceSolve ("solution = " ++ show solution) $ 
          solution
   where -- compute the graph and its transitive closure m
         gr  = buildGraph cs
         n   = nextNode gr            -- number of nodes
         m0  = mkMatrix n (graph gr)
         m   = warshall m0

         -- tracing only: build output version of transitive graph
         legend i = fromJust $ Map.lookup i (intMap gr) -- trace only
         lm0 = LegendMatrix m0 legend legend            -- trace only
         lm  = LegendMatrix m legend legend             -- trace only

         -- compute the sets of flexible and rigid node numbers
         ns  = Map.keys (nodeMap gr)                    
         -- a set of flexible variables
         flexs  = foldl (\ l k -> case k of (Flex i) -> i : l
                                            (Rigid _) -> l) [] ns
         -- a set of rigid variables
         rigids = foldl (\ l k -> case k of (Flex _) -> l
                                            (Rigid i) -> i : l) [] ns

         -- rigid matrix indices
         rInds = foldl (\ l r -> let Just i = Map.lookup (Rigid r) (nodeMap gr)
                                 in i : l) [] rigids

         -- check whether there is a solution
         -- d   = [ m!(i,i) | i <- [0 .. (n-1)] ]  -- diagonal
-- a rigid variable might not be less than it self, so no -.. on the 
-- rigid part of the diagonal
         solvable = all (\ x -> x >= oone) [ m!(i,i) | i <- rInds ] &&
-- a rigid variable might not be bounded below by infinity or
-- bounded above by a constant
-- it might not be related to another rigid variable
           all (\ (r,  r') -> r == r' || 
                let Just row = (Map.lookup (Rigid r)  (nodeMap gr))
                    Just col = (Map.lookup (Rigid r') (nodeMap gr))
                    edge = m!(row,col)
                in  isBelow r edge r' ) 
             [ (r,r') | r <- rigids, r' <- rigids ]
           &&
-- a flexible variable might not be strictly below a rigid variable
           all (\ (x, v) -> 
                let Just row = (Map.lookup (Flex x)  (nodeMap gr))
                    Just col = (Map.lookup (Rigid (RVar v)) (nodeMap gr))
                    edge = m!(row,col)
                in  edge >= Finite 0)
             [ (x,v) | x <- flexs, (RVar v) <- rigids ]


         inScope :: FlexId -> Rigid -> Bool
         inScope x (RConst _) = True
         inScope x (RVar v)   = case Map.lookup x (flexScope gr) of
                     Just scope -> scope v
                     Nothing    -> error $ "Warshall.inScope panic: flexible " ++ show x ++ " does not carry scope info when assigning it rigid variable " ++ show v  

{- loop1

while flexible variables and rigid rows left
  find a rigid variable row i
    for all flexible columns j
      if i --n--> j with n<=0 (meaning i + n <= j) then 
        add i + n to the solution of j

-}

         loop1 :: [Rigid] -> Solution -> Maybe Solution
         loop1 (r:rgds) subst = loop1 rgds subst' where 
            row = fromJust $ Map.lookup (Rigid r) (nodeMap gr)
            subst' =
                  foldl (\ sub f -> 
                          let col = fromJust $ Map.lookup (Flex f) (nodeMap gr)
                          in  case (True -- inScope f r  -- SEEMS WRONG TO IGNORE THINGS NOT IN SCOPE
                                   , m!(row,col)) of
--                                Finite z | z <= 0 -> 
                                (True, Finite z) -> 
                                   let trunc z | z >= 0 = 0
                                            | otherwise = -z
                                   in extendSolution sub f (sizeRigid r (trunc z))
                                _ -> sub
                     ) subst flexs       
         loop1 [] subst = case flexs List.\\ (Map.keys subst) of
            [] -> Just subst
            flexs' -> loop2 flexs' subst

{- loop2

while flexible variables j left
  search the row j for entry i
    if j --n--> i with n >= 0 (meaning j <= i + n) then j = i 

-}
         loop2 :: [FlexId] -> Solution -> Maybe Solution
         loop2 [] subst = Just subst 
         loop2 (f:flxs) subst = loop3 0 subst
           where row = fromJust $ Map.lookup (Flex f) (nodeMap gr)
                 loop3 col subst | col >= n = 
                   -- default to infinity
                    loop2 flxs (extendSolution subst f (SizeConst Infinite)) 
                 loop3 col subst =
                   case Map.lookup col (intMap gr) of
                     Just (Rigid r) | not (infinite r) -> 
                       case (True -- inScope f r
                            ,m!(row,col)) of
                        (True, Finite z) | z >= 0 -> 
                            loop2 flxs (extendSolution subst f (sizeRigid r z))
                        (_, Infinite) -> loop3 (col+1) subst 
                        _ -> Nothing 
                     _ -> loop3 (col+1) subst

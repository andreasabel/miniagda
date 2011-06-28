{- A data structure to represent a forest of upside down trees,
similar to union-find.  The idea is to manage a tree-shaped form of
strict inequations

  i1 > i2 > i3
     > j2 > j3 > j4 > j5
          > k3
          > l3 > l4

  m1 > m2
  
  n1

Checking inequalty x < y is then performed by just enumerating the
parents of x and checking wether y is a member of it.

2010-11-12 UPDATE: We generalize this to ">=" and more by attaching to 
each link a non-negative number.

  0  means  >=
  1  means  >
  n  means  at least n units greater
-}

module TreeShapedOrder where

import Data.List hiding (insert) -- groupBy

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Tree (Tree(..), Forest) -- rose trees
import qualified Data.Tree as Tree

import Util -- headM

-- | Tree-structured partial orders.
--   Represented as maps from children to parents plus a non-negative distance.
newtype TSO a = TSO { unTSO :: Map a (Int,a) } deriving (Eq, Ord)

-- | Empty TSO.
empty :: TSO a
empty = TSO $ Map.empty

-- | @insert a b o@  inserts a with parent b into order o.
-- It does not check whether the tree structure is preserved. 
insert :: (Ord a, Eq a) => a -> (Int, a) -> TSO a -> TSO a
insert a b (TSO o) = TSO $ Map.insert a b o

-- | Construction from a list of child-distance-parent tuples.
fromList :: (Ord a, Eq a) => [(a,(Int,a))] -> TSO a
fromList l = foldl (\ o (a,b) -> insert a b o) empty l

-- | @parents a0 o = [(d1,a1),..,(dn,an)]@ lists the parents of @a0@ in order,
-- i.e., a(i+1) is parent of a(i) with distance d(i+1).
parents :: (Ord a, Eq a) => a -> TSO a -> [(Int,a)]
parents a (TSO o) = loop (Map.lookup a o) where
  loop Nothing  = []
  loop (Just (n,b)) = (n,b) : loop (Map.lookup b o)

-- | @parent a o@ returns the immediate parent, if it exists.
parent :: (Ord a, Eq a) => a -> TSO a -> Maybe (Int,a)
parent a t = headM $ parents a t 

-- | @isAncestor a b o = Just n@ if there are n steps up from a to b.  
isAncestor :: (Ord a, Eq a) => a -> a -> TSO a -> Maybe Int
isAncestor a b o = loop 0 ((0,a) : parents a o)
   where loop acc [] = Nothing
         loop acc ((n,a) : ps) | a == b    = Just (acc + n)
                               | otherwise = loop (acc + n) ps

-- | @diff a b o = Just k@ if there are k steps up from a to b  
-- or (-k) steps down from b to a.
diff ::  (Ord a, Eq a) => a -> a -> TSO a -> Maybe Int
diff a b o = maybe (fmap (\ k -> -k) $ isAncestor b a o) Just $ isAncestor a b o

-- | create a map from parents to list of sons, leaves have an empty list  
invert :: (Ord a, Eq a) => TSO a -> Map a [(Int,a)]
invert (TSO o) = Map.foldWithKey step Map.empty o where
  step son (dist, parent) m = Map.insertWith (++) son [] $ 
    Map.insertWith (++) parent [(dist, son)] m

-- | @height a t = Just k@ if $k$ is the length of the 
--   longest path from @a@ to a leaf. @Nothing@ if @a@ not in @t@.
height :: (Ord a, Eq a) => a -> TSO a -> Maybe Int
height a t = do
  let m = invert t
  let loop parent = do
        sons <- Map.lookup parent m
        return $ if null sons then 0 else 
                  maximum $ map (\ (n,son) -> maybe 0 (n +) $ loop son) sons
  loop a

-- | get the leaves of the TSO forest
leaves :: (Ord a, Eq a) => TSO a -> [a]
leaves o = map fst $ filter (\ (parent,sons) -> null sons) $ Map.toList (invert o)

{- FLAWED BOTTOM-UP-ATTEMPT, DOES NOT WORK
{- How to invert a TSO?

1. Create a Map from parents to their list of children.

2. Keep a working set of nodes.  
   Find the leafs in this working set (nodes that do not have children).
   Cluster them by their parents.
   Turn their parents into trees,
   Continue with the parents.
-}
-- | invert a tree shaped order into a forest.  This can be used for printing
toForest :: (Ord a, Eq a) => TSO a -> Forest a
toForest o = loop (step initialTrees) where 
  initialTrees = map (flip Node []) $ leaves o
  -- step :: (Ord a, Eq a) => Forest a -> [(Maybe a, Forest a)]
  step ts = map (\ l -> (fst (head l), map snd l)) $
    groupBy (\ (p,t) (p',t') -> p == p') $ 
    sortBy (\ (p,t) (p',t') -> compare p p') $ 
    map (\ t -> (parent (rootLabel t) o, t)) ts 
  -- loop :: (Ord a, Eq a) => [(Maybe a, Forest a)] -> Forest a
  loop [] = []
  -- the trees whose roots have no parents are parts of the final forest 
  loop ((Nothing, roots) : nonroots) = roots ++ loop nonroots
  -- the trees whose roots have a parent are iterated
  loop nonroots = loop $ step $ map (\ (Just p, ts) -> Node p ts) nonroots
-}

-- take a lexicographically sorted list of pathes
-- and turn it into a forest by
-- gathering the lists by common prefixes
pathesToForest :: (Ord a, Eq a) => [[(Int,a)]] -> Forest (Int, a)
pathesToForest [] = []
pathesToForest ll =
  map (\ l -> Node (head (head l)) 
                   (pathesToForest $ filter (not . null) $ map tail l)) $ 
    groupBy (\ l l' -> head l == head l') ll

-- | invert a tree shaped order into a forest.  This can be used for printing.
toForest :: (Ord a, Eq a) => TSO a -> Forest (Int,a)
toForest o = pathesToForest $ sort $ map (\ a -> reverse ((0,a) : parents a o)) $ leaves o -- lex. sort

instance (Ord a, Eq a, Show a) => Show (TSO a) where
  show o = Tree.drawForest $ map (fmap show) $ toForest o 

{-
draw :: (Ord a, Eq a, Show a) => TSO a -> String
draw o = Tree.drawForest $ map (fmap show) $ toForest o 
-}

-- test

l1 = map (\ (k,l) -> ("i" ++ show k, (1, "i" ++ show l))) [(0,1),(1,2),(2,3),(3,4)]
     ++ [("j2",(1,"i3"))]
o1 = fromList l1
t1 = diff "i2" "i1" o1
t2 = diff "i2" "j2" o1
t3 = height "i2" o1
t4 = height "i4" o1
t5 = height "k" o1
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Collection where

import Data.List as List
import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

class Monoid c => Collection c e | c -> e where
{-
  empty     :: c
  append    :: c -> c -> c
  concat    :: [c] -> c
-}
  singleton :: e -> c
  delete    :: e -> c -> c
  (\\)      :: c -> c -> c

instance Eq a => Collection [a] a where
{-
  empty     = []
  append    = (++)
  concat    = List.concat
-}
  singleton = (:[])
  delete    = List.delete
  (\\)      = (List.\\)

instance Ord a => Collection (Set a) a where
{-
  empty     = Set.empty
  append    = Set.union
  concat    = Set.unions
-}
  singleton = Set.singleton
  delete    = Set.delete
  (\\)      = (Set.\\)

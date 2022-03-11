-- 2010-03-28 Exists and Bracket via parametric function types
-- 2012-01-22 parameters gone from constructors

data Sigma (A : Set)(B : A -> Set) : Set
{ pair : (fst : A) -> (snd : B fst) -> Sigma A B
}

data Subset (A : Set)(B : A -> Set) : Set
{ put : (get : A) -> [prf : B get] -> Subset A B
}

data Exists (A : Set)(B : A -> Set) : Set
{ exI : [a : A] -> (prop : B a) -> Exists A B
}

fun exE : [A : Set] -> [B : A -> Set] -> [C : Set] ->
      Exists A B -> ([a : A] -> B a -> C) -> C
{ exE A B C (exI a b) k = k a b
}

data Bracket (A : Set) : Set
{ bI : [a : A] -> Bracket A
}

fun bE : [A : Set] -> [C : Set] -> Bracket A -> ([A] -> C) -> C
{ bE A C (bI a) k = k a
}


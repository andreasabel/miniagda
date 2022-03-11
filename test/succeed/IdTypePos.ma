-- 2010-06-20

data Id ++(A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

data Exists (A : Set) ++(P : A -> Set) : Set
{ exI : (witness : A) -> (proof : P witness) -> Exists A P
}

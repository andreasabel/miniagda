-- 2010-06-11, Nisse

data Box (A : Set) : Set
{ box : A -> Box A
}

data Neg : Set
{ neg : (Box Neg -> Neg) -> Neg
}

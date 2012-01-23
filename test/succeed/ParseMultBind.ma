let K : (A, B : Set) -> Set
    = \ A B -> A

data Prod ++(A, B : Set) : Set
{ pair : A -> B -> Prod A B
}

fun fst : [A, B : Set] -> Prod A B -> A
{ fst A B (pair a b) = a
}

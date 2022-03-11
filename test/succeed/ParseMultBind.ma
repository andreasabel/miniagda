let K : (A, B : Set) -> Set
    = \ A B -> A

data Prod ++(A, B : Set) : Set
{ pair : A -> B -> Prod A B
}

fun fst : [A, B : Set] -> Prod A B -> A
{ fst A B (pair a b) = a
}

-- 2012-02-04 telescopes in pi types
fun snd : [A, B : Set] (p : Prod A B) -> B
{ snd A B (pair a b) = b
}

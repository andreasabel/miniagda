-- 2013-04-05

data Bool { false ; true }

fun If : Bool -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

data D (A, B : Set)
{ c : D (If true A B) (If false A B) }

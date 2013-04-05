-- 2012-02-01

-- Prelude

data Empty {}
data Unit { unit }
data Bool { true; false }

fun If : (b : Bool) -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

let Either ++(A, B : Set) = (b : Bool) & If b B A
pattern left  a = (false, a)
pattern right b = (true, b)

let Maybe ++(A : Set) = Either Unit A
pattern nothing = left unit
pattern just a  = right a

-- Nat

cofun Nat : +Size -> Set
{ Nat i = Maybe ([j < i] & Nat j)
}
pattern zero  = nothing
pattern suc n = just n

fun pred : [i : Size] -> Nat i -> Nat i
{ pred i zero             = zero
; pred i (suc (j < i, n)) = n
}

-- Lists

cofun List : ++(A : Set) -> +(i : Size) -> Set
{ List A i = Maybe (A & [i' < i] & List A i')
}
pattern nil      = nothing
pattern cons a l = just (a, l)

-- append (list -> list -> list)

fun append : [A : Set] ->
             [i : Size] -> |i| -> List A i ->
             [j : Size] -> List A j -> List A (i+j)
{ append A i nil                 j bs = bs
; append A i (cons a (i'<i, as)) j bs = cons a (i'+j, append A i' as j bs)
}

-- drop (list -> list)

fun drop : [A : Set ] -> Nat # ->
           [j : Size] -> List A j -> List A j
{ drop A zero j l                     = l
; drop A n    j nil                   = nil
; drop A n    j (cons a (j' < j, as)) = drop A (pred # n) j' as
}

-- take for lists is take for colists after embedding


-- Colists

cofun CoList : ++(A : Set) -> -(i : Size) -> Set
{ CoList A i = Maybe (A & ([i' < i] -> CoList A i'))
}
-- pattern conil      = nil
-- pattern cocons a s = cons a as

-- take (colist -> list)

fun take : [A : Set] -> [i : Size] -> Nat i -> CoList A i -> List A i
{ take A i zero as = nil
; take A i n   nil = nil
; take A i (suc (i' < i, n)) (cons a l) = cons a (i', take A i' n (l i'))
}

-- colist (list -> colist)

fun colist : [A : Set] -> [i : Size] -> |i| -> List A i -> CoList A i
{ colist A i nil                   = nil
; colist A i (cons a (i' < i, as)) = cons a (\ i'' -> colist A (max i' i'') as)
}

-- codrop (colist -> colist)

fun codrop : [A : Set ] ->
             [i : Size] -> Nat i ->
             [j : Size] -> CoList A (j+i) -> CoList A j
{ codrop A i zero              j l          = l
; codrop A i n                 j nil        = nil
; codrop A i (suc (i' < i, n)) j (cons a l) = codrop A i' n j (l (j+i'))
}

-- append (colist -> colist -> colist)

cofun coappend : [A : Set] -> [i : Size] -> |i| ->
                 CoList A i -> CoList A i -> CoList A i
{ coappend A i nil        bs = bs
; coappend A i (cons a l) bs = cons a (\ i' -> coappend A i' (l i') bs)
}

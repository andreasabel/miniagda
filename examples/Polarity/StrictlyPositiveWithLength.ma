{-
Robert Crebbers on the Coq list, 2012-05-09

I am wondering why Coq does not allow the following inductive type.

  Inductive val : Type :=
    | vnat : nat -> val
    | varray (l : list val) : length l > 0 -> val.
-}

data Nat
{ zero
; suc (pred : Nat)
}

data Lt : Nat -> Nat -> Set
{ ltZ (   y : Nat)              : Lt zero    (suc y)
; ltS (x, y : Nat)(lt : Lt x y) : Lt (suc x) (suc y)
}

data List ++(A : Set)
{ nil
; cons (head : A) (tail : List A)
}

fun length : [A : Set] -> List A -> Nat
{ length A nil         = zero
; length A (cons a as) = suc (length A as)
}

data Val
{ vnat   (nat : Nat)
; varray (list : List Val) (nonEmpty : Lt zero (length Val list))
}

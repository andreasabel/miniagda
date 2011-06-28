data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data List : Set
{ nil : List
; cons : Nat -> List -> List
}

fun sumList : List -> Nat
{ sumList nil = zero
; sumList (cons zero l) = sumList l
; sumList (cons (succ x) l) = succ (sumList (cons x l))
}

fun sumList' : List -> Nat
{ sumList' nil = zero
; sumList' (cons x nil) = x
; sumList' (cons x (cons zero l)) = sumList' (cons x l)
; sumList' (cons x (cons (succ y) l)) = succ (sumList' (cons x (cons y l)))
}

fun sumList'' : List -> Nat
{ sumList'' nil = zero
; sumList'' (cons x nil) = x
; sumList'' (cons x (cons zero l)) = sumList'' (cons x l)
; sumList'' (cons x (cons (succ y) l)) = sumList'' (cons (succ x) (cons y l))
}

fun sumList''' : List -> Nat
{ sumList''' nil = zero
; sumList''' (cons zero nil) = zero
; sumList''' (cons (succ x) nil) = succ (sumList''' (cons x nil))
; sumList''' (cons x (cons zero l)) = sumList''' (cons x l)
; sumList''' (cons x (cons (succ y) l)) = sumList''' (cons (succ x) (cons y l))
}

fun sumList'''' : List -> Nat
{ sumList'''' nil = zero
; sumList'''' (cons zero nil) = zero
; sumList'''' (cons (succ x) nil) = succ (sumList'''' (cons x nil))
; sumList'''' (cons x (cons zero l)) = sumList'''' (cons x l)
; sumList'''' (cons x (cons (succ y) l)) = sumList'''' (cons (succ (succ x)) (cons y l))
}
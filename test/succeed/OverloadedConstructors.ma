-- 2013-04-26

data Nat { zero ; suc (n : Nat) }

let one : Nat = suc zero
let two : Nat = suc one

fun add : Nat -> Nat -> Nat
{ add zero n = n
; add (suc m) n = suc (add m n)
}

data Fin (n : Nat)
-- refines Nat
{ zero            : Fin (suc n)
; suc (i : Fin n) : Fin (suc n)
}

fun weakF1 : [m : Nat] -> Fin m -> Fin (suc m)
-- refines \ i -> i
{ weakF1 (.suc m) zero    = zero
; weakF1 (.suc m) (suc i) = suc (weakF1 m i)
}

fun weakF : (n : Nat) [m : Nat] -> Fin m -> Fin (add n m)
-- refines \ i -> i
{ weakF zero    m i = i
; weakF (suc n) m i = weakF1 (add n m) (weakF n m i)
}

fun addF : (n : Nat) [m : Nat] -> Fin n -> Fin m -> Fin (add n m)
-- refines add
{ addF (.suc n) m zero    j = weakF (suc n) m j
; addF (.suc n) m (suc i) j = suc (addF n m i j)
}


data List ++(A : Set) { nil ; cons (x : A) (xs : List A) }

fun lookupL : [A : Set] (i : Nat) (xs : List A) -> A
{ lookupL A zero    (cons x xs) = x
; lookupL A (suc i) (cons x xs) = lookupL A i xs
}

data Vec ++(A : Set) (n : Nat)
-- refines List
{ nil                              : Vec A zero
; cons (head : A) (tail : Vec A n) : Vec A (suc n)
}

fun lookup : [A : Set] [n : Nat] (i : Fin n) (xs : Vec A n) -> A
-- refines LookupL
{ lookup A (.suc n) zero    (.cons x xs) = x
; lookup A (.suc n) (suc i) (.cons x xs) = lookup A n i xs
}

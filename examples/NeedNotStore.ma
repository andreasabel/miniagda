data Nat { zero; suc (n : Nat) }

data Vec (A : Set) (n : Nat)
{ nil                         : Vec A zero
; cons (a : A) (as : Vec A n) : Vec A (suc n)
}

data Fin (n : Nat)
{ fzero            : Fin (suc n)
; fsuc (i : Fin n) : Fin (suc n)
}

fun lookup : [A : Set] -> [n : Nat] -> (i : Fin n) -> (xs : Vec A n) -> A
{ lookup A (.suc n) fzero    (cons x xs) = x
; lookup A (.suc n) (fsuc i) (cons x xs) = lookup A n i xs
}

data Prod (A, B : Set)
{ pair : (fst : A) -> (snd : B) -> Prod A B
} fields fst, snd

fun mypair : Prod Nat Nat
{ mypair .fst = zero
; mypair .snd = suc zero
}

{-

fst : (A, B : Set) -> Prod A B -> A
fst (pair A B a b) --> a
snd (pair A B a b) --> b

mypair : Prod Nat Nat.
fst Nat Nat mypair --> zero
snd Nat Nat mypair --> suc zero.

-}

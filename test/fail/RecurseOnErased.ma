data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

-- matching on irrelevant arguments needs to be forbidden
fun f : [Nat] -> Nat -> Nat
{ f zero n = n  -- this should not be allowed!
; f m zero = zero
; f m (succ n) = n
}

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

-- because of irrelevance of first argument of f
-- this should hold:
let p1 : (n : Nat) -> Id Nat (f zero n) (f (succ zero) n)
       = \ n -> refl Nat (f zero n)


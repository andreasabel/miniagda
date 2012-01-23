data Id (A : Set) (a : A) : A -> Set 
{ refl : Id A a a
}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{ add zero     y = y
; add (succ x) y = succ (add x y) 
}

data Vec (A : Set) : Nat -> Set
{ nil  : Vec A zero
; cons : [n : Nat] -> A -> Vec A n -> Vec A (succ n)
}

{- 2012-01-23 BROKEN
fun length : [A : Set] -> [n : Nat] -> Vec A n -> < n : Nat >
{ length A .zero     (nil)        = zero
; length A .(succ n) (cons n a v) = succ (length A n v)
}
-}

fun head : [A : Set] -> [n : Nat] -> Vec A (succ n) -> A 
{ head A .n (cons n a v) = a
}
fun tail : [A : Set] -> [n : Nat] -> Vec A (succ n) -> Vec A n
{ tail A .n (cons n a v) = v
}

fun repeat : [A : Set] -> (a : A) -> (n : Nat) -> Vec A n
{ repeat A a zero     = nil 
; repeat A a (succ n) = cons n a (repeat A a n)
}


data Fin : Nat -> Set
{ fzero : [n : Nat] -> Fin (succ n)
; fsucc : [n : Nat] -> Fin n -> Fin (succ n)
}

fun lookup : [A : Set] -> [n : Nat] -> Vec A n -> Fin n -> A
{ lookup A .(succ n) (cons n a v) (fzero .n)   = a
; lookup A .(succ n) (cons n a v) (fsucc .n i) = lookup A n v i
; lookup A .zero     (nil)        ()  -- IMPOSSIBLE
}

fun lookupRepeat : [A : Set] -> [a : A] -> (n : Nat) -> (i : Fin n) ->
                   Id A a (lookup A n (repeat A a n) i) 
{ lookupRepeat A a (succ n) (fzero .n) = refl
; lookupRepeat A a (succ n) (fsucc .n i) = lookupRepeat A a n i
; lookupRepeat A a zero () -- IMPOSSIBLE
}

fun downFrom : (n : Nat) -> Vec Nat n
{ downFrom zero     = nil
; downFrom (succ n) = cons n n (downFrom n)
}

fun castVec : [n, m : Nat] -> Id Nat n m -> [A : Set] -> Vec A n -> Vec A m
{ castVec n .n refl A v = v
}

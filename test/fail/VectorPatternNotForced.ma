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

fun length : [A : Set] -> [n : Nat] -> Vec A n -> < n : Nat >
{ length A .zero     nil          = zero
; length A .(succ n) (cons n a v) = succ (length A n v)
}

fun head : [A : Set] -> [n : Nat] -> Vec A (succ n) -> A 
{ head A .n (cons n a v) = a
}
fun tail : [A : Set] -> [n : Nat] -> Vec A (succ n) -> Vec A n
{ tail A .n (cons n a v) = v
}

fun zeroes : (n : Nat) -> Vec Nat n
{ zeroes zero     = nil 
; zeroes (succ x) = cons x zero (zeroes x)
}

data Fin : Nat -> Set
{ fzero : [n : Nat] -> Fin (succ n)
; fsucc : [n : Nat] -> Fin n -> Fin (succ n)
}

fun lookup : [A : Set] -> [n : Nat] -> Vec A n -> Fin n -> A
{ lookup A .(succ n) (cons n a v) (fzero .n)   = a
; lookup A .(succ n) (cons n a v) (fsucc .n i) = lookup A n v i
; lookup A .zero     nil        ()  -- IMPOSSIBLE
}

-- the following should give an error, since we cannot match on [n : Nat]
fun downFrom : [n : Nat] -> Vec Nat n
{ downFrom zero     = nil
; downFrom (succ n) = cons n n (downFrom n)
}
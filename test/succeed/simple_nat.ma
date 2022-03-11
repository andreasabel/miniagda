data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{ add zero x = x
; add (suc y) x = suc (add y x)
}


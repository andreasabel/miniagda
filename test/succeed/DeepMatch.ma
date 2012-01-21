
data Nat : Set 
{ Z : Nat
; S : Nat -> Nat
}

fun plus : Nat -> Nat -> Nat
{ plus Z m = m
; plus (S n) m = S (plus n m)
}

fun fib : Nat -> Nat
{ fib Z         = S Z
; fib (S Z)     = S Z
; fib (S (S n)) = plus (fib n) (fib (S n))
}
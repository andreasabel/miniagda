-- 2010-09-14

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

-- wrong use of parameter
data D (n : Nat) : Set
{ d0 : D zero
}
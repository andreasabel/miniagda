data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

data Bool : Set
{ true  : Bool
; false : Bool
}

fun leq : Nat -> Nat -> Bool
{ leq zero n = true
; leq (suc m) zero = false
; leq (suc m) (suc n) = leq m n
}

fun maxN : Nat -> Nat -> Nat
{ maxN n m = case leq n m
  { true -> m
  ; false -> n
  }
}

let one : Nat = suc zero
let two : Nat = suc one
eval let cmp : Bool = leq one two
eval let bla : Nat = maxN one two

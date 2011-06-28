data Bool : Set
{ true : Bool
; false : Bool
}

fun not : Bool -> Bool
{ not true = false
; not false = true
}

data Nat : Bool -> Set
{ zero : Nat true
; succ : (b : Bool) -> Nat b -> Nat (not b)
}

fun f : (b : Bool) -> [Nat b] -> Bool
{ f true zero = true
; f false (succ n) = false
} 
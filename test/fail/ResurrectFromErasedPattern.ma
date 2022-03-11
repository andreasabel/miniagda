data Bool : Set
{ true : Bool
; false : Bool
}

data Nat : Bool -> Set
{ zero : Nat true
; succ : [b : Bool] -> Nat b -> Nat false
}

fun f : (b : Bool) -> [Nat b] -> Nat false
{ f true zero = succ true zero
; f false (succ b n) = succ false (succ b n)
}

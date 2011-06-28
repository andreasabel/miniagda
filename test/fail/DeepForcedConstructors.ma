-- 2010-01-25
-- 2010-07-08 

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
; succ : (b : Bool) -> Nat b -> Nat false
}

fun f : (b : Bool) -> [Nat b] -> Bool
{ f true zero = true
; f false (succ .true zero) = false
} 
-- should not type check, since match "zero" inside (succ ...) is not forced

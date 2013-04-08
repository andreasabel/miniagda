-- 2013-04-08

data Unit { unit }

fun top : Unit -> Unit
{ top .unit = unit }

data Bool { true ; false }

fail
fun not : Bool -> Bool
{ not .true = false
; not false = true
}

data Nat { zero ; suc (n : Nat) }

fun pred : Nat -> Nat
{ pred zero = zero
; pred (.suc x) = x
}

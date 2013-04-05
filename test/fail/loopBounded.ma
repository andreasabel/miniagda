-- 2012-02-04

data Empty {}
data Unit { unit }
data Maybe ++(A : Set) { nothing ; just (a : A) }

cofun Nat : +Size -> Set
{ Nat i = Maybe ([j < i] & Nat j) 
}
pattern zero    = nothing
pattern suc i n = just (i, n)

fun pred : [i : Size] -> Nat $i -> Nat i
{ pred i zero      = zero
; pred i (suc j n) = n
}

{-
fun loop : [i : Size] -> Nat i -> Unit
{ loop i zero      = unit
; loop i (suc j n) = loop j (pred j (suc j n))
}
-}

fun wfix : [A : Size -> Set] (f : [i : Size] -> ([j < i] -> A j) -> A i)  
  [i : Size]  -> |i| -> A i
{ wfix A f i = f i (wfix A f)
}

fail
fun fix : [A : Size -> Set] (f : [i : Size] -> A i -> A $i) [i : Size] -> A i
{ fix A f i = f i (fix A f)
}

let A -(i : Size) = (Nat # -> Nat i) -> Nat #

fun fix : (f : [i : Size] -> A i -> A $i) [i : Size] -> |i| -> A i
{ fix f i = f i (fix f i)
}

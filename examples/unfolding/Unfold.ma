data Unit { unit }

data Bool { true; false }

fun If : (b : Bool) -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

let Either ++(A, B : Set) = (b : Bool) & If b B A
pattern left  a = (false, a)
pattern right b = (true, b)

let Maybe ++(A : Set) = Either Unit A
pattern nothing = left unit
pattern just a  = right a

cofun Nat : +Size -> Set
{ Nat i = Maybe ([j < i] & Nat j)
}
pattern zero  = nothing
pattern suc n = just n

let succ [i : Size] (n : Nat i) : Nat $i unfold Nat =
  -- unfold Nat in
  -- suc (i, n)
  just (i, n)

fun caseNat : [i : Size] -> |i| -> (n : Nat $i) ->
  [C : Set] -> C -> ([i : Size] -> (m : Nat i) -> C) -> C
unfold Nat
{ caseNat i zero          C z s = z
; caseNat i (suc (i', n)) C z s = s i' n
}

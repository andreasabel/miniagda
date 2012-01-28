-- 2012-01-28  MiniAgda Prelude, PiSigma style

data Empty {}
data Unit { unit }
data Bool { true; false }

fun If : (b : Bool) -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

let Maybe ++(A : Set) : Set
  = (b : Bool) & If b A Unit
pattern nothing = (false, unit)
pattern just a  = (true, a)

cofun Nat : +Size -> Set
{ Nat i = [j < i] & Maybe (Nat j)
}
pattern zero j   = (j, nothing)
pattern succ j n = (j, just n)

let zer [i : Size]             : Nat $i = zero i
let suc [i : Size] (n : Nat i) : Nat $i = succ i n

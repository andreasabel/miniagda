-- 2012-01-28  MiniAgda Prelude, PiSigma style

data Empty {}
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
{ Nat i = [j < i] & Maybe (Nat j)
}
pattern zero j   = (j, nothing)
pattern succ j n = (j, just n)

let zer [i : Size]             : Nat $i = zero i
let suc [i : Size] (n : Nat i) : Nat $i = succ i n

fun plus : [i : Size] -> (n : Nat i) -> 
           [j : Size] -> (m : Nat j) -> Nat (i+j)
{ plus i (zero i')   j m = m
; plus i (succ i' n) j m = succ (i'+j) <| plus i' n j m
}
-- 2012-02-01 type checker turns var pattern i' into size pattern (i' < i)
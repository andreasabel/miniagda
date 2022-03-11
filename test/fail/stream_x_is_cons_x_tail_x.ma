
data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat
}

let n0 : Nat = zero
let n1 : Nat = succ n0
let n2 : Nat = succ n1
let n3 : Nat = succ n2
let n4 : Nat = succ n3


sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

fun tail : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{
  tail A i (cons .i x xs) = xs
}

cofun bad : (i : Size) -> Stream Nat i
{
  bad ($ ($ i)) = cons _ n0 (tail Nat _ (bad ($ i)))
}

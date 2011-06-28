
sized codata Stream (+ A : Set) : Size -> Set {
  cons : [i : Size] -> A -> Stream A i -> Stream A ($ i)
}
 
data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat 
}

-- the following needs to be rejected
-- the matching on size is illegal since the target is not Stream Nat i
cofun copyFirst : (i : Size) -> Stream Nat i -> Stream Nat ($ i)
{ copyFirst ($ i) (cons .Nat .i x xs) = cons Nat ($ i) x (cons Nat i x xs)
}

cofun ones : (i : Size) -> Stream Nat i
{ ones ($ i) = copyFirst i (ones i)
}
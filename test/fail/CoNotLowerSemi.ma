data Nat +(i : Size)
{ zero
; suc (jn : [j < i] & Nat j)
}

data Stream ++(A : Set) -(i : Size)
{ cons (head : A) (tail : [j < i] -> Stream A j)
}

cofun repeat : [A : Set] (a : A) [i : Size] |i| -> Stream A i
{ repeat A a i = cons a (\ j -> repeat A a j)
}

-- infinite tuples not lsc

let lsc (s : Stream (Nat #) #) : [j < #] & Stream (Nat j) #
  = (#, s)

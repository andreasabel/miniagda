data Nat +(i : Size)
{ zero
; suc (jn : [j < i] & Nat j)
}

codata Stream ++(A : Set)
{ cons (head : A) (tail : Stream A)
}

-- infinite tuples not lsc

let lsc (s : Stream (Nat #)) : [j < #] & Stream (Nat j)
  = (#, s)

{-
cofun repeat : [A : Set] (a : A) [i : Size] |i| -> Stream A i
{ repeat A a i = cons a (\ j -> repeat A a j)
}
-}

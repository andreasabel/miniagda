-- 2014-03-08 Using continuation to avoid Either
-- 2011-12-20 Andreas, gcd example using generic Either

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; suc  : [i : Size] -> Nat i -> Nat ($ i)
}

let Either +(A, B : Set) = [C : Set] -> (A -> C) -> (B -> C) -> C

-- subtracting two numbers with minus yields the difference
-- plus a bit indicating the bigger number of the two

fun minus : [i,j : Size] -> |i| -> Nat i -> Nat j -> Either (Nat i) (Nat j)
{ minus i j (zero (i > i'))   m                 left right = right m
; minus i j (suc  (i > i') n) (zero (j > j'))   left right = left  (suc i' n)
; minus i j (suc  (i > i') n) (suc  (j > j') m) left right = minus i' j' n m
 left right
}

-- greated common divisor (gcd)
-- distinguishing cases using cps

fun gcd : [i,j : Size] -> |i,j| -> Nat i -> Nat j -> Nat # -- (max i j)
{ gcd i j (zero (i > i'))  m                = m
; gcd i j (suc (i > i') n) (zero (j > j'))  = suc i' n
; gcd i j (suc (i > i') n) (suc (j > j') m) = minus i' j' n m
  (\ n' -> gcd i' j n' (suc j' m))
  (\ m' -> gcd i j' (suc i' n) m')
}

-- 2011-12-20 Andreas, gcd example using generic Either

sized data Nat : Size -> Set 
{ zero : [i : Size] -> Nat ($ i)
; suc  : [i : Size] -> Nat i -> Nat ($ i)
}

-- Either datatype, specialized to sized Nat

data Either ++(A, B : Set) : Set
{ left  : A -> Either A B
; right : B -> Either A B
}

{- UNUSED
fun either : [A,B,C : Set] -> Either A B ->
     (A -> C) -> (B -> C) -> C
{ either A B C (left  n) l r = l n
; either A B C (right n) l r = r n
}
-}

-- subtracting two numbers with minus yields the difference
-- plus a bit indicating the bigger number of the two

fun minus : [i,j : Size] -> Nat i -> Nat j -> Either (Nat i) (Nat j)
{ minus i j (zero (i > i'))   m               = right m 
; minus i j (suc  (i > i') n) (zero (j > j')) = left  (suc i' n)
; minus i j (suc  (i > i') n) (suc  (j > j') m) = minus i' j' n m
}

-- greated common divisor (gcd) 
-- distinguishing cases using either

fun gcd : [i,j : Size] -> Nat i -> Nat j -> Nat (max i j)
{ gcd i j (zero (i > i')) m = m
; gcd i j (suc (i > i') n) (zero (j > j')) = suc i' n
; gcd i j (suc (i > i') n) (suc (j > j') m) = 
  case minus i' j' n m
  { (left  n') -> gcd i' j n' (suc j' m)
  ; (right m') -> gcd i j' (suc i' n) m'
  }
}

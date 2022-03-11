-- 2011-12-16 Andreas, gcd example

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; suc  : [i : Size] -> Nat i -> Nat ($ i)
}

-- subtracting two numbers with minus yields the difference
-- plus a bit indicating the bigger number of the two

data Either : +Size -> +Size -> Set
{ left  : [i,j : Size] -> Nat i -> Either i j
; right : [i,j : Size] -> Nat j -> Either i j
}

fun minus : [i,j : Size] -> Nat i -> Nat j -> Either i j
{ minus i j (zero (i > i'))   m                 = right i j m
; minus i j (suc  (i > i') n) (zero (j > j'))   = left i j (suc i' n)
; minus i j (suc  (i > i') n) (suc  (j > j') m) = minus i' j' n m
}

{- UNUSED
fun esuc : [i,j : Size] -> Either i j -> Either $i $j
{ esuc i j (left  .i .j n) = left  $i $j (suc i n)
; esuc i j (right .i .j n) = right $i $j (suc j n)
}
-}

-- greated common divisor (gcd) in "Agda-with" style
-- uses an auxiliary function distinguishing over the result of subtraction
-- note how bounded quantification is used to maintain size relations

mutual {

  fun gcd : [i,j : Size] -> Nat i -> Nat j -> Nat (max i j)
  { gcd i j (zero (i > i')) m = m
  ; gcd i j (suc (i > i') n) (zero (j > j')) = suc i' n
  ; gcd i j (suc (i > i') n) (suc (j > j') m) =
      gcd_aux i j i' j' n m (minus i' j' n m)
  }

  fun gcd_aux : [i,j : Size] -> [i' < i] -> [j' < j] -> Nat i' -> Nat j' ->
                Either i' j' -> Nat (max i j)
  { gcd_aux i j i' j' n m (left  .i' .j' n') = gcd i' j n' (suc j' m)
  ; gcd_aux i j i' j' n m (right .i' .j' m') = gcd i j' (suc i' n) m'
  }

}

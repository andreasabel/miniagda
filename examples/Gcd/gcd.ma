

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; suc  : [i : Size] -> Nat i -> Nat $i
}

data MinDiff : +Size -> +Size -> Set
{ pair : [i : Size] -> (min : Nat i) ->
         [j : Size] -> (diff : Nat j) -> MinDiff i j
} fields min, diff

fun sucMD : [i, j : Size] -> MinDiff i j -> MinDiff $i j
{ sucMD i j (pair .i m .j d) = pair $i (suc i m) j d
}

fun minDiff : [i : Size] -> Nat i -> [j < i] -> Nat j -> MinDiff j i
{ minDiff i (zero (k < i))  j (zero (l < j))   = pair j (zero l) i (zero l)
; minDiff i (zero (k < i))  j (suc  (l < j) m) = pair j (zero l) i (suc l m)
; minDiff i n               j (zero (l < j))   = pair j (zero l) i n
; minDiff i (suc (k < i) n) j (suc  (l < j) m) =
    sucMD l (max k j) (minDiff (max k j) n l m)
}

{-
fun gcd : [i : Size] -> Nat i -> Nat i -> Nat i
{ gcd i (zero (k < i)) m = m
; gcd i n (zero (k < i)) = n
; gcd i (suc (k < i) n) (suc (l < i) m) = case minDiff (max k l) n m
  { pair .(max k l) min diff = gcd (max k l)

-}

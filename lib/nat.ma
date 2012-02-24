-- nat.ma

-- Natural numbers

cofun Nat : +Size -> Set
{ Nat i = Maybe ([j < i] & Nat j)
}
pattern zero  = nothing
pattern suc n = just n

let succ [i : Size] (n : Nat i) : Nat $i = suc (i, n)

let oneN   : Nat 1 = suc (0, zero)
let twoN   : Nat 2 = suc (1, oneN)
let threeN : Nat 3 = suc (2, twoN)
let fourN  : Nat 4 = suc (3, threeN)

fun caseNat : [i : Size] -> |i| -> (n : Nat $i) -> 
  [C : Set] -> C -> ([i : Size] -> (m : Nat i) -> C) -> C
{ caseNat i zero          C z s = z
; caseNat i (suc (i', n)) C z s = s i' n
}

{- ERROR in TypeChecker!
fun caseNat : [i : Size] -> |i| -> (n : Nat $i) -> 
  [C : [j : Size] -> Nat j -> Set] ->
  C 0 zero ->
  ([i : Size] -> (m : Nat i) -> C i m) ->
  C $i n
{ caseNat i zero          C z s = z
; caseNat i (suc (i', n)) C z s = s i' n
}
-}

fun iterNat : [A : Set](f : A -> A)(a : A)[i : Size](n : Nat i) -> A
{ iterNat A f a i zero          = a
; iterNat A f a i (suc (i', n)) = iterNat A f (f a) i' n
}

fun pred : [i : Size] -> (n : Nat $i) -> Nat i
{ pred i zero = zero
; pred i (suc (j, n)) = n
}

fun plus : [i : Size] -> |i| -> (n : Nat i) -> 
           [j : Size] ->        (m : Nat j) -> Nat (i+j)
{ plus i zero          j m = m
; plus i (suc (i', n)) j m = suc (i'+j, plus i' n j m)
}

fun times : [i : Size] -> |i| -> (n : Nat i) -> (m : Nat #) -> Nat #
{ times i zero m = zero
; times i (suc (i', n)) m = plus # m # (times i' n m)
}

fun minus : [i : Size] ->        (n : Nat i) -> 
            [j : Size] -> |j| -> (m : Nat j) -> Nat i
{ minus i zero          j m             = zero
; minus i n             j zero          = n
; minus i (suc (i', n)) j (suc (j', m)) = minus i' n j' m
}

-- computes ceil(n/(m+1))
fun div' : [i : Size] -> |i| -> (n : Nat i) -> (m : Nat #) -> Nat i
{ div' i zero          m = zero
; div' i (suc (i', n)) m = suc (i', div' i' (minus i' n # m) m)
}

-- computes floor(n/m) if m>0, and 0 otherwise
check  -- Alternative definition 
  let div [i : Size] (n : Nat i) (m : Nat #) : Nat i
    = caseNat # m (Nat i) 
        zero 
        (\ oo pred_m -> div' i (minus i n # pred_m) pred_m)

check  -- Alternative definition 
  fun div : [i : Size] -> (n : Nat i) -> (m : Nat #) -> Nat i
  { div i n zero = zero
  ; div i n m    = div' i (minus i n # (pred # m)) (pred # m)
  }

-- computes floor(n/m) if m>0, and 0 otherwise
fun div : [i : Size] -> (n : Nat i) -> (m : Nat #) -> Nat i
{ div i n zero          = zero
; div i n (suc (j, m')) = div' i (minus i n # m') m'
}

-- Comparing natural numbers

let Compare +(i, j : Size) = Tri (Nat i) Unit (Nat j)
pattern greater n = first n
pattern equal     = second unit
pattern less m    = third m

-- compares two numbers and returns the difference
fun compare : [i : Size] -> |i| -> (n : Nat i) ->
              [j : Size] ->        (m : Nat j) -> Compare i j
{ compare i zero          j zero          = equal
; compare i n             j zero          = greater n
; compare i zero          j m             = less m
; compare i (suc (i', n)) j (suc (j', m)) = compare i' n j' m
}

-- greatest common divisor
fun gcd : [i : Size] -> (n : Nat i) ->
          [j : Size] -> (m : Nat j) -> |i,j| -> Nat (max i j)
{ gcd i zero          j m             = m
; gcd i n             j zero          = n
; gcd i (suc (i', n)) j (suc (j', m)) = case compare i' n j' m
  { (equal)      -> suc (i', n)
  ; (greater n') -> gcd i' n' j (suc (j', m))
  ; (less m')    -> gcd i (suc (i', n)) j' m'
  }
}


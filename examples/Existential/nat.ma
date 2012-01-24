data Maybe ++(A : Set) : Set
{ nothing : Maybe A
; just    : A -> Maybe A
}

-- sized type and constructors

cofun Nat : +Size -> Set
{ Nat i = [j < i] & Maybe (Nat j)
}

pattern pzero i   = i , nothing
pattern psucc i n = i , just n

fun zero : [i : Size] -> Nat $i
{ zero i = pzero i
}

fun succ : [i : Size] -> Nat i -> Nat $i
{ succ i n = psucc i n
}

-- Fact :       [j < $i] & Maybe (Nat j) -> Nat $i
-- subtype of   Maybe (Nat i) -> Nat $i
-- 
-- General case:
--   T i = [j < i] & F (T j)
-- 
-- Is  F (T i) <= [j <= i] & F (T j)  ?
-- Clearly, if F is monotone!

-- using infinity

fun add : [i : Size] -> |i| -> Nat i -> Nat # -> Nat #
{ add i (pzero j)   m = m
; add i (psucc j n) m = succ # (add j n m)
}

fun pred : [i : Size] -> Nat i -> Nat i
{ pred i (pzero j)   = zero j  
; pred i (psucc j n) = n
}

fun minus : [i : Size] -> |i| -> Nat i -> Nat # -> Nat i
{ minus i (pzero j)   m           = zero j
; minus i (psucc j n) (pzero k)   = succ j n
; minus i (psucc j n) (psucc k m) = minus j n m
}

-- div n m computes floor(n/m+1)
fun div   : [i : Size] -> |i| -> Nat i -> Nat # -> Nat i
{ div i (pzero j)   m = zero j
; div i (psucc j n) m = succ j (div j n (minus j n m))
}

-- fixed-point and constructors without infinity

let NAT : Set = [i : Size] & Nat i

fun Zero : NAT
{ Zero = ($0, zero 0)
}

fun Succ : NAT -> NAT
{ Succ (i, n) = ($i, succ i n)
}

fun plus : [i : Size] -> Nat i -> NAT -> NAT
{ plus i (pzero j)   m = m
; plus i (psucc j n) m = Succ (plus j n m)
}
-- 2010-09-02

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; succ : [i : Size] -> Nat i -> Nat $i
}

{-
fun ack : Nat # -> Nat # -> Nat #
{ ack zero     m        = succ m
; ack (succ n) zero     = ack n (succ zero)
; ack (succ n) (succ m) = ack n (ack (succ n) m) 
}
-}

fail -- 2012-02-05 this fails because size vars are not < #
fun ack : [i, j : Size] -> |i,j| -> Nat i -> Nat j -> Nat #
{ ack .$i j   (zero i)    m         = succ # m
; ack .$i .$j (succ i n) (zero j)   = ack i # n (succ # (zero #))
; ack .$i .$j (succ i n) (succ j m) = ack i # n (ack $i j (succ i n) m) 
}
fun ack : [i, j : Size] -> |i,j| -> Nat i -> Nat j -> Nat #
{ ack i j (zero (i' < i))    m                = succ # m
; ack i j (succ (i' < i) n) (zero (j' < j))   = ack i' # n (succ # (zero #))
; ack i j (succ (i' < i) n) (succ (j' < j) m) = ack i' # n (ack i j' (succ i' n) m) 
}
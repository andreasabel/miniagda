sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

fun Pred : (i : Size) -> (x : Nat ($ i)) -> Set
{ Pred i (succ .i n) = Nat i
; Pred i (zero .i)   = Nat ($ i)
}

fun pred : [i : Size] -> (x : Nat ($ i)) -> Pred i x
{ pred i (succ .i n) = n
; pred i (zero .i)   = zero i
}

{- DOES NOT WORK
fun minus : [i : Size] -> Nat i -> Nat # -> Nat i
{ minus i n (zero .#) = n
; minus i n (succ .# m) = minus i (pred i n) m 
}
-}
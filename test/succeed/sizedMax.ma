sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

check fun maxN : [i : Size] -> Nat i -> Nat i -> Nat i 
{ maxN .($ i) (zero .i) (zero i)   = zero i
; maxN .($ i) (zero .i) (succ i n) = succ i n
; maxN .($ i) (succ .i n) (zero i) = succ i n
; maxN .($ i) (succ .i n) (succ i m) = succ i (maxN i n m)
}
 
fun maxN : [i : Size] -> Nat i -> Nat i -> Nat i 
{ maxN i (zero (i > j)  ) (zero (i > k)  ) = zero j
; maxN i (zero (i > j)  ) (succ (i > k) m) = succ k m
; maxN i (succ (i > j) n) (zero (i > k)  ) = succ j n
; maxN i (succ (i > j) n) (succ (i > k) m) = succ (max j k) 
                                            (maxN (max j k) n m)
}

{-
-- termination checker

  max j k ?<? i

-- constaint solving with max?

; maxN i (succ (i > j) n) (succ (i > k) m) = succ _X (maxN _Y n m)

  _X + 1 <= i
  j <= _Y
  k <= _Y
  _Y <= _X
  
Needs to be solved as _X = _Y = max j k 
-}

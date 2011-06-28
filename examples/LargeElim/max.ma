sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

fun max2 : [i : Size] -> Nat i -> Nat i -> Nat i
{ max2 i (zero (i > j))    n               = n
; max2 i  m               (zero (i > j))   = m
; max2 i (succ (i > j) m) (succ (i > k) n) = 
   let [h : Size] = max j k 
   in  succ h (max2 h m n)
--   succ (max j k) (max2 (max j k) m n)
}

fun Maxs : Nat # -> Size -> Set
{ Maxs (zero .#  ) i = Nat i
; Maxs (succ .# n) i = Nat i -> Maxs n i
}

fun maxs : (n : Nat #) -> [i : Size] -> Nat i -> Maxs n i
{ maxs (zero .#)   i m = m
; maxs (succ .# n) i m = \ l -> maxs n i (max2 i m l)
}
 

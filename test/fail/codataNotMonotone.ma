-- 2010-05-06

sized data SNat : Size -> Set 
{ zero : [i : Size] -> SNat ($ i)
; succ : [i : Size] -> SNat i -> SNat ($ i) 
}

sized codata NatEq : (i : Size) -> SNat i -> SNat i -> Set
{ eqz : [i : Size] -> NatEq ($ i) (zero i) (zero i)
; eqs : [i : Size] -> (n : SNat i) -> (m : SNat i) -> 
   NatEq i n m -> NatEq ($ i) (succ i n) (succ i m)
}

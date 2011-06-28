-- 2010-03-11 size functions

data Bool : Set
{ true : Bool
; false : Bool
}

data N : Set
{ zz : N
; ss : N -> N
}

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

let infty : Size = #
let ssuc : Size -> Size = \ i -> $ i

fun maybeSuc : (b : Bool) -> Size -> Size
{ maybeSuc true i = $ i
; maybeSuc false i = i
}

fun addSize : N -> Size -> Size
{ addSize zz i = i
; addSize (ss n) i = $ (addSize n i)
}

fun addSNat : (n : N) -> (i : Size) -> Nat i -> Nat (addSize n i)
{ addSNat zz     i m = m
; addSNat (ss n) i m = succ (addSize n i) (addSNat n i m) 
}


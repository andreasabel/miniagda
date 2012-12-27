
sized data NatS : Size -> Set
{ zeroS : [i : Size] -> NatS ($ i)
; succS : [i : Size] -> NatS i -> NatS ($ i)
}

fun maxi : [i : Size] -> NatS i -> [j : Size] -> NatS j -> NatS (max i j)
{ maxi .($ i) (succS i n) .($ j) (succS j m) = succS (max i j) (maxi i n j m)
; maxi .($ i) (zeroS i)   .($ j) (succS j m) = succS (max i j) m
; maxi .($ i) (succS i n) .($ j) (zeroS j)   = succS (max i j) n
; maxi .($ i) (zeroS i)   .($ j) (zeroS j)   = zeroS (max i j)
}

fun maxima : [i, j : Size] -> NatS i -> NatS j -> NatS (max i j)
{ maxima i j (zeroS (i' < i))    m                 = m
; maxima i j  n                 (zeroS (j' < j))   = n
; maxima i j (succS (i' < i) n) (succS (j' < j) m) = succS _ (maxima _ _ n m)
}

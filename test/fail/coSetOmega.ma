cofun D : (i : Size) -> CoSet i
{ D ($ i) = D i -> D i
}

let sapp : D # -> D # 
    = \ x -> x x

eval let omega : D # -> D #
               = sapp sapp


sized data SNat : Size -> Set
{ zero : [i : Size] -> SNat ($ i)
; succ : [i : Size] -> SNat i -> SNat ($ i)
}

data MaybeNat (i : Size) : Set
{ nothing : MaybeNat i
; just    : SNat i -> MaybeNat i
}

fun pred' : [i : Size] -> SNat ($ i) -> MaybeNat i
{ pred' i (succ .i n) = just i n
; pred' i (zero .i)   = nothing i
}

fun pred : (i : Size) -> SNat ($$ i) -> SNat ($ i)
{ pred i (succ .($ i) n) = n
; pred i (zero .($ i))   = zero i
}
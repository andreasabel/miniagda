data Maybe (i : Size) (A : CoSet i) : CoSet ($ i)
{ nothing : Maybe i A
; just    : A -> Maybe i A
}

cofun Nat : (i : Size) -> CoSet i
{ Nat ($ i) = Maybe i (Nat i)
}


let zero : Nat #
         = nothing -- # (Nat #)

let succ : Nat # -> Nat #
         = \ n -> just n -- just # (Nat #) n

{-
fun iter' : [A : Set] -> (A -> A) -> A -> Nat # -> A
{ iter' A f a (nothing .# .(Maybe # (Nat #))) = a
; iter' A f a (just .# .(Maybe # (Nat #)) n)  = f (iter' A f a n)
}
-}

fun iter : [A : Set] -> (A -> A) -> A -> Nat # -> A
{ iter A f a  nothing = a
; iter A f a (just n)  = f (iter A f a n)
}

{-
let zero : [i : Size] -> Nat ($ i)
         = \ i -> nothing i (Nat i)

let succ : [i : Size] -> Nat i -> Nat ($ i)
         = \ i -> \ n -> just i (Nat i) n
-}


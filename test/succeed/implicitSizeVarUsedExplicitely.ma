data Bool : Set
{ true : Bool
; false : Bool
}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun leq : Nat -> Nat -> Bool {}

fun plus : [A : Set] -> A -> A -> A {}

sized data List : Size -> Set
{ nil  : (i : Size) -> List ($ i) 
; cons : [i : Size] -> Nat -> List i -> List ($ i)
}

fun filter : [i : Size] -> List i -> List i
{ filter .($ i) (nil i) = nil i  -- Size variables are resurrected
; filter .($ i) (cons i n l) = plus (List ($ i)) (filter _ l) (cons _ n (filter _ l))
}

fun quicksort : [i : Size] -> List i -> List #
{ quicksort .($ i) (nil i) = nil _
; quicksort .($ i) (cons i n l) = 
    plus (List #) (quicksort _ (filter i l)) (cons _ n (quicksort _ (filter i l))) 
}

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}
{-
let p1 : (i : Size) -> Id (List #) (nil i) (nil #)
       = \ i -> refl (List #) (nil i)
-}
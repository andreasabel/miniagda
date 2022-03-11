-- sized Nat

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

let sizeIrr : (i : Size) -> Id (Nat #) (zero i) (zero #)
    = \ i -> refl

-- sized rose trees
-- with polymorphic quantification (2010-01-02)

data List (+ A : Set) : Set
{ nil  : List A
; cons : A -> List A -> List A
}

fun mapList : [A : Set] -> [B : Set] -> (A -> B) -> List A -> List B
{ mapList A B f (nil) = nil
; mapList A B f (cons a as) = cons (f a) (mapList A B f as)
}

sized data Rose (+ A : Set) : Size -> Set
{ rose : [i : Size] -> A -> List (Rose A i) -> Rose A ($ i)
}

fun mapRose : [A : Set] -> [B : Set] -> (A -> B) ->
              [i : Size] -> Rose A i -> Rose B i
{ mapRose A B f .($ i) (rose i a rs) =
  rose i (f a) (mapList (Rose A i) (Rose B i) (mapRose A B f i) rs)
}

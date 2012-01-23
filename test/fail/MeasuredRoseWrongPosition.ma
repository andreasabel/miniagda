-- 2010-07-27

data List (+ A : Set) : Set
{ nil  : List A
; cons : A -> List A -> List A
}

fun mapList : [A : Set] -> [B : Set] -> (A -> B) -> List A -> List B
{ mapList A B f (nil) = nil
; mapList A B f (cons a as) = cons (f a) (mapList A B f as)
}

-- sized Roses

sized data Rose (+ A : Set) : Size -> Set
{ rose : [i : Size] -> A -> List (Rose A i) -> Rose A ($ i) 
}

fun mapRose : [A : Set] -> [B : Set] -> (A -> B) -> 
              [i : Size] -> Rose A i -> |i| -> Rose B i
{ mapRose A B f i (rose (i > j) a rs) = 
    rose j (f a) (mapList (Rose A j) (Rose B j) (mapRose A B f j) rs)
}
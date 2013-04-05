-- 2012-03-06
-- to avoid the problems of equi-recursive types (non-terminating eta)
-- we keep constructors

data Stream ++(A : Set) -(i : Size) : Set
{ cons (head : A) (tail : [j < i] -> Stream A j)
} fields head, tail

fun tail' : [A : Set] [i : Size] (as : Stream A $i) -> Stream A i
{ tail' A i (cons a s) = s i
}

data SP -(A : Set) ++(B : Set) -(i : Size) +(j : Size) : Set
{ get (f  : A -> [j' < j] & SP A B i j')
; put (b  : B)
      (sp : [i' < i] -> SP A B i' #)
}

cofun eat : [A, B : Set] [i, j : Size] |i,j| -> SP A B i j -> Stream A # -> Stream B i
{ eat A B i j (get f) as = case f (head as)
    { (j', sp) -> eat A B i j' sp (tail' A # as) }
; eat A B i j (put b sp) as = cons b (\ i' -> eat A B i' # (sp i') as)
}

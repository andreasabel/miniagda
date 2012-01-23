-- 2010-11-01
-- 2012-01-22 parameters gone from constructors

sized data List (A : Set) : +Size -> Set
{ nil  : [i : Size] -> List A $i
; cons : [i : Size] -> A -> List A i -> List A $i
}

fun append : [A : Set] -> [i, j : Size] -> List A i -> List A $j -> List A (i + j)
{ append A i j (nil (i > i')) l = l
; append A i j (cons (i > i') a as) l = cons (i' + j) a (append A i' j as l)
}

-- 2010-11-01

sized data List (A : Set) : +Size -> Set
{ nil  : [i : Size] -> List A $i
; cons : [i : Size] -> A -> List A i -> List A $i
}

fun append : [A : Set] -> [i, j : Size] -> List A i -> List A $j -> List A (i + j)
{ append A i j (nil .A (i > i')) l = l
; append A i j (cons .A (i > i') a as) l = cons A (i' + j) a (append A i' j as l)
}

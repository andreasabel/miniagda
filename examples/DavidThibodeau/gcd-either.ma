sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; suc  : [i : Size] -> Nat i -> Nat ($ i)
}

data Either : +Size -> +Size -> Set
{ left  : [i,j : Size] -> Nat i -> Either i j
; right : [i,j : Size] -> Nat j -> Either i j
}

fun esuc : [i,j : Size] -> Either i j -> Either $i $j
{ esuc i j (left  .i .j n) = left  $i $j (suc i n)
; esuc i j (right .i .j n) = right $i $j (suc j n)
}

fun minus : [i,j : Size] -> Nat i -> Nat j -> Either i j
{}

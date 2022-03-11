-- 2010-11-01

data Nat : Set
{ zero : Nat
; suc : Nat -> Nat
}

fun toSize : Nat -> Size
{ toSize zero = 0
; toSize (suc n) = $(toSize n)
}

fun toSizeNT : Nat -> Size
{ toSizeNT zero = 0
; toSizeNT (suc n) = $(toSizeNT (suc n))
}

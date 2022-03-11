sized data SNat : Size -> Set
{ zero : [i : Size] -> SNat ($ i)
; succ : [i : Size] -> SNat i -> SNat ($ i)
}

fun minus : [i : Size] -> SNat i -> SNat # -> SNat i
{   minus i (zero (i > j))     y                 = zero j
;   minus i (succ (i > j) x)   (zero .#)         = succ j x
;   minus i (succ (i > j) x)   (succ .# y)       = minus j x y
}

data Bool : Set
{ true : Bool
; false : Bool
}

fun isSmaller : [i : Size] -> SNat i -> SNat i -> Bool
{ isSmaller i (zero (i > j)) y = true
; isSmaller i (succ (i > j) x) (zero (i > k)) = false
; isSmaller i (succ (i > j) x) (succ (i > k) y) = isSmaller (max j k) x y
}

fun branch : [i : Size] -> Bool -> SNat i -> SNat i -> SNat i
{ branch i true a b = a
; branch i false a b = b
}

fun gcd : [i : Size] -> [j : Size] -> SNat i -> SNat j -> SNat (max i j)
{ gcd i j (zero (i > k))   y                = y
; gcd i j (succ (i > k) x) (zero (j > l))   = succ k x
; gcd i j (succ (i > k) x) (succ (j > l) y) =
    branch (max i j) (isSmaller (max i j) x y)
           (gcd i l (succ k x) (minus l y x))
           (gcd k j (minus k x y) (succ l y))

}

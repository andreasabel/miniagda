-- 2012-01-31  Examples for FICS 2012 talk
-- 2012-02-05

-- Recursion principle

sized codata CoList ++(A : Set) : -(i : Size) -> Set
{ nil  [i : Size]                           : CoList A $i
; cons [i : Size] (a : A) (as : CoList A i) : CoList A $i
}

cofun repeat : [A : Set] (a : A) [i : Size] -> CoList A i
{ repeat A a ($ i) = cons i a (repeat A a i)
} 

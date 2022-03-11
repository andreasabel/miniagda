-- 2010-08-18

sized codata CoList (A : Set) : Size -> Set
{ conil  : [i : Size] -> CoList A $i
; cocons : [i : Size] -> A -> CoList A i -> CoList A $i
}

-- the following declaration must be cofun otherwise non-termination
fun repeat : [A : Set] -> (a : A) -> [i : Size] -> CoList A i
{ repeat A a ($ i) = cocons A i a (repeat A a i)
}

data Unit : Set { unit : Unit }
eval let units : CoList Unit # = repeat Unit unit #

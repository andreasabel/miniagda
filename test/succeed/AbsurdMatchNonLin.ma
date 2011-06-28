-- 2010-07-08

data Bool : Set
{ true  : Bool
; false : Bool
}

data BB : Bool -> Set
{ tt : BB true
; ff : BB false
}

data Empty : Set {}
data Unit : Set { unit : Unit }

fun True : Bool -> Set
{ True true  = Unit
; True false = Empty
}

fun not : Bool -> Bool
{ not true = false
; not false = true
}

-- the information that True b is empty is not available early enough
-- if we process left to right
-- works if test for emptiness is postponed till after checking patterns
fun bla : (b : Bool) -> True b -> True (not b) -> BB b -> Empty
{ bla .false () x ff
; bla .true x () tt
}

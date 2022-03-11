-- 2010-09-05
-- Tried to trick MiniAgda into believing that an empty type is a tuple type
-- but it did not follow me.  Good!

data Bool : Set
{ true  : Bool
; false : Bool
}

-- a fake tuple type
data EmptyOr ++(A : Set) : Bool -> Set
{ inn : (out : A) -> EmptyOr A true
}

fun exFalso : [A, B : Set] -> EmptyOr A false -> B
{ exFalso A B ()
}

sized codata Stream ++(A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A $i
}

cofun bla : [A : Set] -> [i : Size] -> EmptyOr (Stream A i) false
{ bla A ($ i) = exFalso (Stream A i) (EmptyOr (Stream A $i) false) (bla A i)
}

fun anything : [A : Set] -> A
{ anything A = exFalso (EmptyOr (Stream Bool #) false) A (bla Bool #)
}

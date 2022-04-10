fun SET : Set 1
{ SET = Set
}

fun TId : Set 1
-- unfold SET
{ TId = (A : SET) -> A -> A }

fun id : TId
-- unfold TId
{ id A x = x }

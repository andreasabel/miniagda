fun SET : Set 1
{ SET = Set
}

fun TId : Set 1
-- Without the following line, this definition should not type-check:
-- unfold SET
{ TId = (A : SET) -> A -> A }

-- Unreached:
fun id : TId
-- unfold TId
{ id A x = x }

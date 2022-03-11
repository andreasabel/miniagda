-- 2010-06-18

-- the following definition needs to be rejected!
data Wrap [A : Set] : Set
{ inn : (out : A) -> Wrap A
}
fields out

fun cast : [A : Set] -> [B : Set] -> A -> B
{ cast A B a = out {- B -} (inn {- A -} a)
}

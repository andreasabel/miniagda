
sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

-- the type of this identity is not the type of a fun
fun sid : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{
  sid A i (cons .i x xs) = cons _ x (sid A _ xs)
}
-- size constraints unsolvable


sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

-- tail is fine since it is non-recursive, so the type need not be
-- admissible 
fun tail : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{
  tail A i (cons .i x xs) = xs
}

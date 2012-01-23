-- 2012-01-22 parameters gone from constructors

sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

cofun sid : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{
  sid A ($ i) (cons .($ i) x xs) = cons _ x (sid A i xs)
}

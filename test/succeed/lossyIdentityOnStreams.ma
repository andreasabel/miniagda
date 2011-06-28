
sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

cofun sid : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{
  sid A ($ i) (cons .A .($ i) x xs) = cons A _ x (sid A i xs)
}

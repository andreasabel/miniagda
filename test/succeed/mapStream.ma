-- 2012-01-22 parameters gone from constructors

sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}

cofun map : (A : Set) -> (B : Set) -> (i : Size) -> 
            (A -> B) -> Stream A i -> Stream B i 
{
  map A B ($ i) f (cons .i x xl) = cons _ (f x) (map A B _ f xl)
}

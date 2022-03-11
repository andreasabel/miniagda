data P (A : Set) : (A -> A) -> Set
{
  inn : (out : A -> A) -> P A out
}

fun bla : (A : Set) -> (f : (A -> A) -> (A -> A)) ->
  P (A -> A) f ->  P (A -> A) (\ x -> f x)
{
  bla A f p = p    -- (c .(A -> A) f) = c (A -> A) (\ x -> f x)
}

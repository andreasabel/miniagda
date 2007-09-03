data TransMu (A : Set) ( B : Set) (X : Set) : Set
{
  put : B -> X -> TransMu A B X;
  get : (A -> TransMu A B X) -> TransMu A B X
}

fun Trans : Set -> Set -> Set
{
Trans = \A -> \B -> TransMu A B (Trans A B) 
}
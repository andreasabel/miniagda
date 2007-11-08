-- not consistent
-- const x : Set = Set

data W ( A : Set) : (A -> Set) -> Set 
{
  sup : ( B : A -> Set) -> (a : A) -> ( B a -> W A B) -> W A B
}
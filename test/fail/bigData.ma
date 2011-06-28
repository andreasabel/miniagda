-- 2010-06-25 removed Set:Set, so this should not pass

data Any : Set 
{ inn : (Out : Set) -> Any }

data Big : Set -> Set
{
  big : (A : Set) -> (B : Set) -> Big (A -> B)
}

fun bla : (A : Set) -> Big A -> Big A
{
  bla .(A -> B) (big A B) = big A B
--  bla (.(A) -> .(B)) (big A B) = big A B
}
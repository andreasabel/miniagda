
sized data SNat : Size -> Set
{
  zero : (i : Size) -> SNat ($ i);
  succ : (i : Size) -> SNat i -> SNat ($ i)
}

sized codata Stream : Size -> Set {
  cons : (i : Size) -> SNat # -> Stream i -> Stream ($ i)
}

-- This is a fake lexicographic induction on (j,i)

-- 2010-03-10: size pattern in co constructors must be dotted
-- but the pattern ($ j) fails since target is not Stream j
fun bla : (i : Size) -> SNat i -> (j : Size) -> Stream j -> (A : Set) -> A
{
  bla .($ i) (zero i) ($ j) (cons .j x xs) = bla # x j xs ;
  bla .($ i) (succ i y)   j            xs  = bla i y j xs
}
-- 2010-08-18: ($ j) only in cofun

-- OLD:
-- Analysis declares j unusable for termination, so termination check fails
fun blo : (i : Size) -> SNat i -> (j : Size) -> Stream j -> (A : Set) -> A
{
  blo .($ i) (zero i) .($ j) (cons j x xs) = blo # x j xs ;
  blo .($ i) (succ i y)   j            xs  = blo i y j xs
}
-- NEW:
-- size patterns in coconstructors must be dotted!
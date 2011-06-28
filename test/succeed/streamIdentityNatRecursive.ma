
sized data SNat : Size -> Set
{
  zero : (i : Size) -> SNat ($ i);
  succ : (i : Size) -> SNat i -> SNat ($ i)
}

sized codata Stream : Size -> Set {
  cons : (i : Size) -> SNat # -> Stream i -> Stream ($ i)
}


-- a silly stream identity
-- this is refuted by Karl's overly restrictive admissibility test

fun sid : (i : Size) -> SNat i -> (j : Size) -> Stream j -> Stream j
{
  sid .($ i) (zero i)   j xs = xs ;
  sid .($ i) (succ i y) j xs = sid i y j xs
}

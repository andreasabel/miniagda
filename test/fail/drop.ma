
sized data SNat : Size -> Set
{
  zero : (i : Size) -> SNat ($ i);
  succ : (i : Size) -> SNat i -> SNat ($ i)
}

sized codata Stream : Size -> Set {
  cons : (i : Size) -> SNat # -> Stream i -> Stream ($ i)
}

-- drop the first elements of a stream

cofun drop : (i : Size) -> SNat i -> (j : Size) -> Stream j -> Stream j
{
  drop .($ i) (zero i)   j       xs           = xs ;
  drop .($ i) (succ i y) ($ j) (cons .j x xs) = drop i y j xs
}

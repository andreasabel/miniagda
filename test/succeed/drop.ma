
sized data SNat : Size -> Set
{
  zero : (i : Size) -> SNat ($ i);
  succ : (i : Size) -> SNat i -> SNat ($ i)
}

sized codata Stream : Size -> Set {
  cons : (i : Size) -> SNat # -> Stream i -> Stream ($ i)
}

-- drop the first elements of a stream

fun drop : (i : Size) -> SNat i -> Stream # -> Stream #
{
  drop .($ i) (zero i)    xs           = xs ;
  drop .($ i) (succ i y) (cons .# x xs) = drop i y xs
}

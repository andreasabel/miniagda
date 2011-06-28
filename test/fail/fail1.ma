-- rigid variable clash

sized data SNat : Size -> Set
{
zero : (i : Size) -> SNat ($ i);
succ : (i : Size) -> SNat i -> SNat ($ i)
}

fun inc : (i : Size) -> (j : Size) -> SNat i -> SNat j
{
inc i j x = succ _ x;
}

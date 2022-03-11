
sized data SNat : Size -> Set
{
zero : (i : Size) -> SNat ($ i);
succ : (i : Size) -> SNat i -> SNat ($ i)
}

fun f : (i : Size) -> SNat i -> SNat #
{
f ($ ($ i)) x = f ($ i) (zero i)
}


sized data SNat : Size -> Set
{
zero : (i : Size) -> SNat ($ i);
succ : (i : Size) -> SNat i -> SNat ($ i)
}

data Empty : Set
{
}

-- ($ i) appearing as a size pattern

fun bad : (i : Size) -> SNat i -> Empty
{
bad .($ i)     (succ i x)   = bad _ x;
bad .($ ($ i)) (zero ($ i)) = bad _ (zero _);
}

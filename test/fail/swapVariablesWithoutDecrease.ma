-- termination fails with variable swapping

sized data SNat : Size -> Set
{
zero : (i : Size) -> SNat ($ i);
succ : (i : Size) -> SNat i -> SNat ($ i)
}

fun bla : (i : Size) -> (j : Size) -> SNat i -> SNat j -> SNat #
{
bla .($ i) j (succ i x) y = bla _ _ (succ _ y) x;
}

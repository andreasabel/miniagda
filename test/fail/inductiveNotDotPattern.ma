sized data SNat : Size -> Set
{
zero : (i : Size ) -> SNat ($ i);
succ : (i : Size ) -> SNat i -> SNat ($ i)
}

-- no complete pattern matching
cofun bla : (i : Size ) -> SNat ($ i) -> SNat i
{
bla .($ i) (zero ($ i)) = zero _; -- no complete pattern matching
bla .i (succ i x) = x
}

fun loop : (i : Size ) -> (SNat i) -> Set
{
loop ($ i) x = loop _ (bla _ x)
}

-- eval let diverge : Set = loop # (zero #)


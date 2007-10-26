data SNat : Size -> Set
{
zero : (i : Size ) -> SNat ($ i);
succ : (i : Size ) -> SNat i -> SNat ($ i)
}

-- not admissble
norec bla : (i : Size ) -> SNat ($ i) -> SNat i
{
bla .($ i) (zero ($ i)) = zero i;
bla .i (succ i x) = x 
}

fun loop : (i : Size ) -> (SNat i) -> Set
{
loop ($ i) x = loop i (bla i x)
}

eval const diverge : Set = loop # (zero #)

{- -- case nat

nonrec deconstruct_nat : (i : Size) -> SNat ($ i) -> Maybe (SNat i)
{
 deconstruct_nat i (zero i) = nothing;
 deconstruct_nat i (succ i n) = just n
}

-}
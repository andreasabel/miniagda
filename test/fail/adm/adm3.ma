data Maybe (A : Set) : Set
{ nothing : Maybe A
; just : A -> Maybe A
}

sized data SNat : Size -> Set
{
zero : (i : Size ) -> SNat ($ i);
succ : (i : Size ) -> SNat i -> SNat ($ i)
}

-- no complete pattern matching
fun bla : (i : Size ) -> SNat ($ i) -> SNat i
{
bla .($ i) (zero ($ i)) = zero i; -- no complete pattern matching
bla .i (succ i x) = x
}
-- 2010-08-18 new error: successor pattern only allowed in cofun

-- termination check fails because ($ i) is unusable
fun loop : (i : Size ) -> (SNat i) -> Set
{
loop ($ i) x = loop i (bla i x)
}

eval let diverge : Set = loop # (zero #)

fun deconstruct_nat : (i : Size) -> SNat ($ i) -> Maybe (SNat i)
{
 deconstruct_nat i (zero .i) = nothing (SNat i);
 deconstruct_nat i (succ .i n) = just (SNat i) n
}


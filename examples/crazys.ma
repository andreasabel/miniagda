data SNat : Size -> Set
{
zero : (i: Size ) -> SNat ($ i);
succ : (i : Size ) -> SNat i -> SNat ($ i)
}

fun o2e : (i : Size ) -> SNat i -> SNat i
{
o2e .($ i) (zero i) = zero i;
o2e .($ $ i) (succ .($ i) (zero i)) = zero i; 
o2e .($ $ i) (succ .($ i) (succ i x)) = succ ($ i) (succ i (o2e i x ))
}

-- "permutating size arguments"
fun crazy : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat #
{
bla .($ i) j (zero i) y = y;
bla .($ i) j (succ i x) y = succ # (crazy j i y (o2e i x)) 
}

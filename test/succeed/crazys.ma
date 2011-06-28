sized data SNat : Size -> Set
{
zero : (i : Size ) -> SNat ($ i);
succ : (i : Size ) -> SNat i -> SNat ($ i)
}

fun o2e : (i : Size ) -> SNat i -> SNat i
{
o2e .($ i) (zero i) = zero _;
o2e .($ $ i) (succ .($ i) (zero i)) = zero _; 
o2e .($ $ i) (succ .($ i) (succ i x)) = succ _ (succ _ (o2e _ x ))
}

-- "permutating size arguments"
fun crazy : (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat #
{
crazy .($ i) j (zero i) y = y;
crazy .($ i) j (succ i x) y = succ _ (crazy _ _ y (o2e _ x)) 
}

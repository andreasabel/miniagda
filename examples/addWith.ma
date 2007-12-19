sized data SNat : Size -> Set
{
zero : (i: Size ) -> SNat ($ i);
succ : (i : Size ) -> SNat i -> SNat ($ i)
}

-- a size preserving function
fun ote : (i : Size ) -> SNat i -> SNat i
{
ote .($ i) (zero i) = zero i;
ote .($ $ i) (succ .($ i) (zero i)) = zero i; 
ote .($ $ i) (succ .($ i) (succ i x)) = succ ($ i) (succ i (ote i x ))
}

-- "permutating size arguments"
fun addWith : ((k : Size ) -> SNat k -> SNat k ) -> (i : Size ) -> (j : Size ) -> SNat i -> SNat j -> SNat #
{
addWith f .($ i) j (zero i) y = y;
addWith f .($ i) j (succ i x) y = succ # (addWith f j i (f j y) (f i x)) 
}

let 3 : SNat # = succ # (succ # (succ # (zero #))) 
let 4 : SNat # = succ # 3

eval let bla : SNat # = addWith ote # # 4 3 


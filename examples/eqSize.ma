-- not allowed to be a sized type..
data Eq : Size -> Size -> Set
{
	refl : (i : Size) -> Eq i i 
}

--so this does not work with subtyping
const eqSucc : (i : Size ) -> Eq ($ i) i = \j -> refl j 

data Nat : Size -> Set
{
zero : (i : Size ) -> Nat ($ i);
succ : (i : Size ) -> Nat i -> Nat ($ i)
}

fun subst : (i : Size ) -> (j : Size ) -> Eq i j -> Nat i -> Nat j
{
subst .i .i (refl i) x = x 
}

fun smaller : (i : Size ) -> Nat ($ i) -> Nat i 
{
smaller i x = subst ($ i) (i) (eqSucc i) x
}

data Empty : Set
{
}

fun loop : (i : Size ) -> Nat i -> Empty
{
loop .($ i) (zero i) = loop i (smaller i (zero i));
loop .($ i) (succ i x) = loop i (smaller i (succ i x))
}

eval const diverge : Empty = loop # (succ # (zero #))
data Empty : Set
{
}

data Nat : Size -> Set
{
zero : ( i : Size ) -> Nat ($ i);
succ : ( i : Size ) -> Nat i -> Nat ($ i)
}

-- not type correct
fun f : (i : Size ) -> Nat i -> Empty
{
f ($ i) (zero .i) = f i (zero i);
f ($ i) (succ .i _) = f ($ i) (zero i)
}

-- const loop : Empty = f # (zero #)


data Empty : Set
{
}

data Nat : Size -> Set
{
zero : ( i : Size ) -> Nat (s i);
succ : ( i : Size ) -> Nat i -> Nat (s i)
}

fun f : (i : Size ) -> Nat i -> Empty
{
f (s i) (zero .i) = f i (zero i);
f (s i) (succ .i _) = f i (zero i)
}

const loop : Empty = f infty (zero infty)
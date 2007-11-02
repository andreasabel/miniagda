data Nat : Size -> Set
{
zero : ( i : Size ) -> Nat ($ i);
succ : ( i : Size ) -> Nat i -> Nat ($ i)
}

fun foo : (i : Size ) -> Nat i
{
foo ($ i) = foo i
}
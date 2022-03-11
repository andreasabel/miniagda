sized data Nat : Size -> Set
{
zero : ( i : Size ) -> Nat ($ i);
succ : ( i : Size ) -> Nat i -> Nat ($ i);
}


-- size not used
fun foo : (i : Size ) -> Nat i
{
--foo ($ i) = foo i -- subtyping
}


-- 2010-03-10, this is admissible, but not terminating!
fun foo2 : ( i : Size ) -> Nat ($ i) -> Set
{
foo2 i (zero .i) = foo2 i (zero i);
foo2 i (succ .i x) = Nat #
}


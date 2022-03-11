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


-- not inductive in i
fun foo2 : (i : Size ) -> Nat ($ i) -> Set
{
foo2 i (zero .i) = foo2 _ (zero _);
foo2 i (succ .i x) = Nat _
}

-- I think the analysis declares i unusable for termination, and then the check fails

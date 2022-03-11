sized data Nat : Size -> Set
{
zero : ( i : Size ) -> Nat ($ i);
succ : ( i : Size ) -> Nat i -> Nat ($ i)
}

-- 2010-03-10
-- termination checking fails because this pattern is declared unusable
-- it would be clearer if the pattern ($ i) was rejected because
-- Nat i is not coinductive
-- 2010-08-18 now clearer
fun foo : (i : Size ) -> Nat i
{
foo ($ i) = foo i
}

data Eq (A : Set ) ( a : A ) : A -> Set
{
refl : Eq A a a
}

-- K axiom
fun K : ( A: Set ) -> ( a : A ) -> (P : Eq A a a -> Set ) -> P (refl A a) -> (p : Eq A a a) -> P p
{
K .A .a P pr (refl A a) = pr
}


data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat (s i);
	succ : (i : Size ) -> SNat i -> SNat (s i)
}

data Eq (A : Set) (a : A) : A -> Set
{
  refl : Eq A a a
}

fun subst : (A : Set) -> (P : A -> Set) -> (i : A) -> (j : A) ->
            Eq A i j -> P i -> P j
{
  subst A P i i (refl A i) p = p
}

fun h : (ass : (i : Size) -> Eq Size (s i) i) -> (i : Size) -> SNat i -> SNat infty
{
  h ass (s i) (zero i) = h ass i (subst Size SNat (s i) i (ass i) (zero i));
  h ass (s i) (succ i n) = h ass i n
}


const loop : (ass : (i : Size) -> Eq Size (s i) i) -> SNat infty 
           = \ ass -> h ass infty (zero infty) 


-- the following program has to be rejected 
-- because of incomplete pattern matching
fun g : (ass : (i : Size) -> Eq Size (s i) i) -> (i : Size) -> SNat i -> SNat infty
{
  g ass (s i) x = g ass i (subst Size SNat (s i) i (ass i) x)
}

-- const yy : (ass : (i : Size) -> Eq Size (s i) i) -> 
--	     Eq (SNat infty) (zero infty) (g ass infty (zero infty)) 
--         = \ ass -> refl (SNat infty) (zero infty)

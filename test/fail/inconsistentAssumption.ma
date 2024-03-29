sized data SNat : Size -> Set
{
        zero : (i : Size) -> SNat ($ i);
        succ : (i : Size) -> SNat i -> SNat ($ i)
}

data Eq (A : Set) (a : A) : A -> Set
{
  refl : Eq A a a
}

fun subst : (A : Set) -> (P : A -> Set) -> (i : A) -> (j : A) ->
            Eq A i j -> P i -> P j
{
  subst A P i .i (refl) p = p
}

fun h : (ass : (i : Size) -> Eq Size ($ i) i) -> (i : Size) -> SNat i -> SNat #
{
  h ass .($ i) (zero i)   = h ass i (subst Size SNat ($ i) i (ass i) (zero i));
  h ass .($ i) (succ i n) = h ass i n
}


let loop : (ass : (i : Size) -> Eq Size ($ i) i) -> SNat #
         = \ ass -> h ass # (zero #)


-- the following program has to be rejected
-- because of incomplete pattern matching
fun g : (ass : (i : Size) -> Eq Size ($ i) i) -> (i : Size) -> SNat i -> SNat #
{
  g ass ($ i) x = g ass i (subst Size SNat ($ i) i (ass i) x)
}

-- let  yy : (ass : (i : Size) -> Eq Size ($ i) i) ->
--           Eq (SNat #) (zero #) (g ass # (zero #))
--         = \ ass -> refl (SNat #) (zero #)

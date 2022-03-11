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

-- h is not a problem since the right hand side of the first clause
-- does not reduce if ass i is not refl
fun h : (ass : (i : Size) -> Eq Size ($ i) i) -> (i : Size) -> SNat i -> SNat #
{
  h ass .($ i) (zero i)   = h ass i (subst Size SNat ($ i) i (ass i) (zero i));
  h ass .($ i) (succ i n) = h ass i n
}


let loop : (ass : (i : Size) -> Eq Size ($ i) i) -> SNat #
         = \ ass -> h ass # (zero #)

let  yy : (ass : (i : Size) -> Eq Size ($ i) i) ->
             Eq (SNat #) (zero #) (h ass # (zero #))
        = \ ass -> refl

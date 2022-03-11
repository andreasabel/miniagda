data Tm : Set
{ succ : Tm -> Tm
}

sized data Step : Size -> Tm -> Tm -> Set
{ s_succ : [i : Size] -> [n, m : Tm] -> Step i n m -> Step $i (succ n) (succ m)
}

{- Hello,

I am having trouble with the encoding of proofs within Miniagda. I would like first to know if
it is possible to use the empty pattern () as in Agda to indicate that a given case cannot occur or if it is
simply considered unnecessary since there is not coverage check done on the functions.

Also, I would like to define an equality type like
-}

data Eq (n : Tm) : Tm -> Set
{ refl : Eq n n
}

fun cong_succ : [n, m : Tm] -> Eq n m -> Eq (succ n) (succ m)
{ cong_succ n .n refl = refl
}
{-
and used it to prove that small-step semantics is deterministic. In Agda, one would write the following case

det : {m : Tm} -> {n1 : Tm} -> {n2 : Tm} ->
      Step m n1 -> Step m n2 -> Eq n1 n2
det (s_succ t) (s_succ t') with det t t'
... | ref = ref
which is accepted by the typechecker. However, the corresponding code does not work in Miniagda.
-}

fun det : [i : Size] -> [n : Tm] -> [m1 : Tm] -> [m2 : Tm] ->
          Step i n m1 -> Step i n m2 -> Eq m1 m2
{ det i .(succ n) .(succ m1) .(succ m2)
      (s_succ (i > k) .n m1 t) (s_succ (i > l) n m2 t')  =
        cong_succ m1 m2 (det (max k l) n m1 m2 t t')
}
{-
It seems to be because of the necessity to write explicitly an argument for the constructor refl and
that this argument cannot be unified with m1 or m2. Moreover, I have to provide also an argument for the
refl that I want to output that would represent that succ m1 and succ m2 are equal. Is there a way to make it work
or is it simply impossible to encode in Miniagda?

Thank you,

David
-}

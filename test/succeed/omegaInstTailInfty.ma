{- 2013-03-31 On instantiation of quantifiers [i < #] - F i

If F is upper semi-continuous then

  [i < #] -> F i   is a sub"set" of   F #

so we can instantiate i to #.  (Hughes et al., POPL 96; Abel, LMCS 08)

1) Consider the special case

  F i = [j < i] -> G i

If G is antitone we have a decreasing chain

  G 0 >= G 1 >= ...

Since all chains are shorter than #, we have a "fixpoint" G gamma
for some gamma < #.

  F # = [j < #] -> G j = G gamma

  [i < #] -> F i
      = [i < #] -> [j < i] -> G j  (since # is a limit)
      = [j < #] -> G j = G gamma

Anyway, G does not have to have special properties, it is sufficient
that # is a limit, because

  i < #  iff  i + 1 < #

so

  j < i < #  iff j < #

2) Consider the special case

  F i = [j <= i] -> G j

We have

  F # = [j <= #] -> G j
      = G # /\ ([j < #] -> G j)

  [i < #] -> F i
      = [i < #] -> [j <= i] -> G j
      = [j < #] -> G j

So if G is upper semi-continuous, so is F.

-}

cofun Inf : (F : Size -> Set) -(i : Size) -> Set
{ Inf F i = [j < i] -> F j }

-- uses that  [j < i] -> F j  is upper semi-continuous in i
fun uppersemi : [F : Size -> Set] (f : Inf (Inf F) #) -> Inf F #
{ uppersemi F f j  = f # j }
{-
   have f   : [i < #] -> [j < i] -> F j
   show f # : [j < #] -> F j

-}

data Stream +(A : Set) -(i : Size)
{ scons (shead : [j < i] -> A) (stail : [j < i] -> Stream A j)
} fields shead, stail

check
cofun repeat : [A : Set] (a : A) [i : Size] |i| -> Stream A i
{ repeat A a ($ i) = scons (\ j -> a) (\ j -> repeat A a j)
}

check
let tailInf [A : Set] (s : Stream A #) : Stream A #
  = s .stail #


-- front streams

data Front +(A : Set) -(i : Size)
{ cons (head : A) (tail : [j < i] -> Front A j)
} fields head, tail

fun eta : [F : Size -> Set] [i : Size] (f : [j < i] -> F j) [j < i] -> F j
{ eta F i f j = f j }

fun repeat : [A : Set] (a : A) [i : Size] |i| -> Front A i
{ repeat A a i = cons a (repeat A a)
  -- Or:
; repeat A a i = cons a (eta (Front A) i (repeat A a))
}

let tailInf [A : Set] (s : Front A #) : Front A #
  = s .tail #


-- semicontinuity can be used to instantiate quantifiers
-- related to bound normalization

trustme -- only if F upper semi-continuous
let uppersemicont [F : Size -> Set] (f : [i < #] -> F i) : F #
  = f #

trustme --only if F lower semi-continuous
let lowersemicont [F : Size -> Set] (a : F #) : [i < #] & F i
  = (#, a)

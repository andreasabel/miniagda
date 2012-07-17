-- 2012-01-22 parameters gone from constructors

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

fun subst : [A : Set] -> [a : A] -> [b : A] -> [Id A a b] ->
            [P : A -> Set] -> P a -> P b
{ subst A a .a refl P h = h
}


{- This is ok, due to the eta-expansion at identity type

  since  p -->eta refl A a
  both sides reduce and, hence, equality can be shown.

  However, at compile time, the matching against refl cannot be removed,
  because of the non-linearity of subst!
-}

let p1 : [A : Set] -> [a : A] -> [p : Id A a a] ->
         [P : A -> Set] -> (h : P a) ->
         Id (P a) (subst A a a p P h) (subst A a a refl P h)
    = \ A a p P h -> refl

let p2 [A : Set] [a : A] [p : Id A a a] [P : A -> Set] (h : P a) :
         Id (P a) (subst A a a p P h) h
    = refl

-- this one is uncontroversial:
let p3 : [A : Set] -> [a, b : A] -> [p, q : Id A a b] ->
         [P : A -> Set] -> (h : P a) ->
         Id (P b) (subst A a b p P h) (subst A a b q P h)
    = \ A -> \ a b -> \ p q -> \ P -> \ h -> refl -- (P b) (subst A a b p P h)

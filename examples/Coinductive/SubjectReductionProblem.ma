{- 2010-05-28  In MiniAgda with Ripley equality, this succeeds:

   Thorsten, http://sneezy.cs.nott.ac.uk/fplunch/weblog/?p=104

We define recursively a type of Streams:

Stream : Type.
Stream = tag : {'Cons} ; Case tag of {'Cons -> $ Stream }.

here the $ A is the ASCII notation for lifting . We define recursively

ticks : Stream.
ticks = 'Cons, [ticks].
-}

sized codata Stream : Size -> Set
{ cons : [i : Size] -> Stream i -> Stream ($ i)
}

cofun ticks : [i : Size] -> Stream i
{ ticks ($ i) = cons i (ticks i)
}

{- Currently hasn't got a primitive equality type but we can just
   define Leibniz equality:

Eq : (A : Type) -> A -> A -> Type.
Eq = \ A -> \ a -> \ b -> (P : A -> Type) -> P a -> P b.

refl : (A : Type) -> (a : A) -> Eq A a a.
refl = \ A -> \ a -> \ P -> \ p -> p.
-}

data Eq (A : Set)(a : A) : A -> Set
{ refl : Eq A a a
}

{- We can show:

l1 : Eq Stream ticks ('Cons, [ticks]).
l1 = refl Stream ticks.

-- Andreas: this is because ticks unfolds by itself once.
-- In MiniAgda, ticks unfolds only if forced, e.g. when compared to cons...
-}

let l1 : Eq (Stream #) (ticks #) (cons # (ticks #))
       = refl -- (Stream #) (ticks #)

{- We can also show that the recursive functional is a congruence:

l2 : (s : Stream) -> (t : Stream) -> (Eq Stream s t)
-> Eq Stream ('Cons, [s]) ('Cons, [t])
l2 = \ s -> \ t -> \ q -> \ P -> \ p -> q (\ x -> P ('Cons,x)) p.
-}

fun l2 :  (s : Stream #) -> (t : Stream #) -> (Eq (Stream #) s t)
-> Eq (Stream #) (cons # s) (cons # t)
{ l2 s .s (refl {- .(Stream #) .s-}) = refl --(Stream #) (cons # s)
}

{- Putting l1 and l2 together we seem to be able to unfold inside a box

l3 : Eq ('Cons, [ticks]) ('Cons, [('Cons, [ticks])]).
l3 = l2 ticks ('Cons, [ticks]) l1.
-}

let l3 : Eq (Stream #) (cons # (ticks #)) (cons # (cons # (ticks #)))
       = l2 (ticks #) (cons # (ticks #)) l1

let l3' : Eq (Stream #) (cons # (ticks #)) (cons # (cons # (ticks #)))
        = refl -- (Stream #) (ticks #)

{- However, if we reduce l3 to normal form it is basically an instance of refl, i.e.
\ P -> \ p -> p and this has not got l3's type because
the expressions ('Cons, [ticks]) and ('Cons, [('Cons, [ticks])]) are not convertible. Clearly we have lost subject reduction.
-}

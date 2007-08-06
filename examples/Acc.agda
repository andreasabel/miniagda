module Acc where

data Acc ( A : Set ) ( Lt : A -> A -> Set) : A -> Set where
  acc :    (   b : A ) 
        -> ( ( a : A ) -> Lt a b  ->  Acc A Lt a )
        -> ( Acc A Lt b )

data Nat : Set where
  zero : Nat
  succ : Nat -> Nat

data Lt : Nat -> Nat -> Set where
  ltzero : ( x : Nat ) -> Lt zero (succ x)
  ltsucc : ( x : Nat ) -> (y : Nat) -> Lt x y -> Lt (succ x) (succ y)

notLt0 : ( x : Nat ) -> Lt x zero -> (C : Set) -> C
notLt0 x ()

data Eq : Nat -> Nat -> Set where
  eqrefl : ( x : Nat ) -> Eq x x
  eqsucc : ( x y : Nat ) -> Eq x y -> Eq (succ x) (succ y)

data Or (A B : Set ) : Set where
  inl : A -> Or A B
  inr : B -> Or A B 

wkLt : ( x y : Nat ) -> Lt (succ x) (succ y) -> Lt x y
wkLt zero (succ y) _ = ltzero y
wkLt x zero (ltsucc .x .zero ())
wkLt x y (ltsucc .x .y p) = p 

{-
wkLt1 : ( x y : Nat ) -> Lt (succ x) y -> Lt x y
wkLt1 x zero ()
wkLt1 zero (succ y) _ = ltzero y 
wkLt1 (succ x) (succ y) (ltsucc .(succ x)  .y p) = ltsucc x y (wkLt1 x y p)   

wkLt2 : (x y : Nat ) -> Lt x y -> Lt x (succ y)
wkLt2 x zero () 
wkLt2 zero y p = ltzero y 
wkLt2 (succ x) (succ y) (ltsucc .x .y p) = ltsucc x (succ y) (wkLt2 x y p) 
-}

---

help : ( x : Nat ) -> Lt (succ x) (succ zero) -> Eq (succ x) zero
help x p = notLt0 x (wkLt x zero p) (Eq (succ x) zero)

eqOrLt : (x y : Nat ) -> Lt x (succ y) -> Or (Eq x y) (Lt x y)
eqOrLt zero zero p = inl (eqrefl zero)
eqOrLt (succ x) zero p = inl (help x p) 
eqOrLt zero (succ x) p = inr (ltzero x)
eqOrLt (succ x) (succ y) p with eqOrLt x y (wkLt x (succ y) p) 
eqOrLt (succ x) (succ y) p | (inl p2) = inl (eqsucc x y p2)
eqOrLt (succ x) (succ y) p | (inr p2) = inr (ltsucc x y p2)
---

eqP : (x y : Nat ) -> Eq x y -> ( P : Nat -> Set ) -> P y -> P x
eqP x .x (eqrefl .x) P p = p

ltcase : ( P : Nat -> Set ) -> ( x : Nat ) -> ( y : Nat ) ->
         Lt x (succ y) -> ( (x' : Nat ) -> Lt x' y -> P x') -> P y -> P x
ltcase P x y a hx' hy with eqOrLt x y a
ltcase P x y _ hx' hy    | inl eq = eqP x y eq P hy
ltcase P x y _ hx' hy    | inr lt = hx' x lt


accLt : ( x : Nat ) -> Acc Nat Lt x
accLt zero = acc zero (\a -> \p -> notLt0 a p (Acc Nat Lt a) )
accLt (succ x) with accLt x
accLt (succ x) | (acc .x pacc) = acc (succ x) (\a -> \p -> 
                                     ltcase (Acc Nat Lt) a x p pacc (accLt x))


----

data WO ( A : Set ) ( Lt : A -> A -> Set ) : Set where
  wo : ((x : A) -> Acc A Lt x) -> WO A Lt 

woLt : WO Nat Lt
woLt = wo accLt



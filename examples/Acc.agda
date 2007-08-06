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

accLt : ( x : Nat ) -> Acc Nat Lt x
accLt zero = acc zero (\a -> \p -> notLt0 a p (Acc Nat Lt a) )
accLt (succ x) = acc (succ x) (\a -> \p -> (accLt a))

----

data WO ( A : Set ) ( Lt : A -> A -> Set ) : Set where
  wo : ((x : A) -> Acc A Lt x) -> WO A Lt 

woLt : WO Nat Lt
woLt = wo accLt



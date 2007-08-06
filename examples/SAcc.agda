module SAcc where

data Size : Set where
  s : Size -> Size 

data Acc ( A : Set ) ( Lt : A -> A -> Set) : Size -> A -> Set where
  acc :   (i : Size) ->  ( b : A ) 
        -> ( ( a : A ) -> Lt a b  ->  Acc A Lt i a )
        -> ( Acc A Lt (s i) b )

data Nat : Set where
  zero : Nat
  succ : Nat -> Nat

data Lt : Nat -> Nat -> Set where
  ltzero : ( x : Nat ) -> Lt zero (succ x)
  ltsucc : ( x : Nat ) -> (y : Nat) -> Lt x y -> Lt (succ x) (succ y)

notLt0 : ( x : Nat ) -> Lt x zero -> (C : Set) -> C
notLt0 x ()

accLt : (i : Size ) -> ( x : Nat ) -> Acc Nat Lt i x
accLt (s i) zero = acc i zero (\a -> \p -> notLt0 a p (Acc Nat Lt i a) )
accLt (s i) (succ x) = acc i (succ x) (\a -> \p -> (accLt i a))




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

ltcase : ( x : Nat ) -> ( y : Nat ) -> Lt x (succ y) ->  
         ( P : Nat -> Set ) -> ( (x' : Nat ) -> Lt x' y -> P x') -> P y -> P x
ltcase zero zero      _ P hx' hy = hy
ltcase zero (succ y') _ P hx' hy = hx' zero (ltzero y')
ltcase (succ x') zero (ltsucc .x' .zero ()) _ _ _
ltcase (succ x') (succ y') (ltsucc ._ ._ p) P hx' hy = 
  ltcase x' y' p (\ n -> P (succ n))
    (\ x'' p' -> hx' (succ x'') (ltsucc _ _ p')) hy

accSucc : (x : Nat) -> Acc Nat Lt x -> Acc Nat Lt (succ x)
accSucc x a@(acc .x h) = acc (succ x) (\ y p -> ltcase y x p (Acc Nat Lt) h a)


module NotLinearPat where

data Nat : Set where
  zero : Nat
  succ : Nat -> Nat

bad : Nat -> Nat -> Nat
bad x x = zero
bad x y = succ zero


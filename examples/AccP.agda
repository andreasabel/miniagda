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

ltstep : (x : Nat) ->  Lt x (succ x)
ltstep zero = ltzero _
ltstep (succ x) = ltsucc _ _ (ltstep x)

notLt0 : ( x : Nat ) -> Lt x zero -> (C : Set) -> C
notLt0 x ()

wkLt : ( x y : Nat ) -> Lt (succ x) (succ y) -> Lt x y
wkLt zero (succ y) _ = ltzero y
wkLt x zero (ltsucc .x .zero ())
wkLt x y (ltsucc .x .y p) = p 

{-
wkLt1 : ( x y : Nat ) -> Lt (succ x) y -> Lt x y
wkLt1 x zero ()
wkLt1 zero (succ y) _ = ltzero y 
wkLt1 (succ x) (succ y) (ltsucc .(succ x)  .y p) = ltsucc x y (wkLt1 x y p)   
-}

wkLt2 : (x y : Nat ) -> Lt x y -> Lt x (succ y)
wkLt2 x zero () 
wkLt2 zero y p = ltzero y 
wkLt2 (succ x) (succ y) (ltsucc .x .y p) = ltsucc x (succ y) (wkLt2 x y p) 

ltcase : ( x : Nat ) -> ( y : Nat ) -> Lt x (succ y) ->  
         ( P : Nat -> Set ) -> ( (x' : Nat ) -> Lt x' y -> P x') -> P y -> P x
ltcase zero zero      _ P hx' hy = hy
ltcase zero (succ y') _ P hx' hy = hx' zero (ltzero y')
ltcase (succ x') zero (ltsucc .x' .zero ()) _ _ _
ltcase (succ x') (succ y') (ltsucc ._ ._ p) P hx' hy = 
  ltcase x' y' p (\ n -> P (succ n))
    (\ x'' p' -> hx' (succ x'') (ltsucc _ _ p')) hy

accSucc : (x : Nat) -> Acc Nat Lt x -> Acc Nat Lt (succ x)
accSucc x (acc .x h) = acc (succ x) (\ y p -> ltcase y x p (Acc Nat Lt) h (acc x h))


accLt : ( x : Nat ) -> Acc Nat Lt x
accLt zero = acc zero (\a -> \p -> notLt0 a p (Acc Nat Lt a) )
accLt (succ x) = accSucc x (accLt x)


-- subtraction x - y

sub : Nat -> Nat -> Nat
sub zero y = zero
sub (succ x) zero = succ x
sub (succ x) (succ y) = sub x y

subLt : (x y : Nat) -> Lt (sub (succ x) (succ y)) (succ x)
subLt zero y = ltzero _
subLt (succ x') zero = ltstep (succ x')
subLt (succ x') (succ y') = wkLt2 _ _ (subLt x' y')

-- division  x / (y + 1)

div' : (x y : Nat) -> Acc Nat Lt x -> Nat
div' zero _ _ = zero
div' (succ x') y' (acc ._ h) = succ (div' z y' (h _ p)) 
  where z = sub (succ x') (succ y')
        p = subLt x' y'


----

data WO ( A : Set ) ( Lt : A -> A -> Set ) : Set where
  wo : ((x : A) -> Acc A Lt x) -> WO A Lt 

woLt : WO Nat Lt
woLt = wo accLt



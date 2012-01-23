-- 2009-11-29  A few examples about singleton types

let id : (A : Set) -> (x : A) -> <x : A>
       = \ A -> \ x -> x

data Nat : Set 
{ zero : Nat
; succ : Nat -> Nat
}

let zero' 
    : <zero : Nat>
    = zero

let succ'
    : (x : Nat) -> <succ x : Nat>
    = \ x -> succ x

fun pred : [x : Nat] -> <succ x : Nat> -> <x : Nat>
{ pred .x (succ x) = x
} 

-- the recursive constant zero function
fun kzero : (x : Nat) -> <zero : Nat>
{ kzero zero     = zero
; kzero (succ x) = kzero x 
}
-- eta-expansion turns this into the non-recursive
--   kzero x = zero 

data IsZero : Nat -> Set
{ isZero : IsZero zero
} 

let p : (x : Nat) -> IsZero (kzero x)
      = \ x -> isZero
{- Checking works as follows:
  ? x : Nat |- isZero : IsZero (kzero x)
  ? IsZero zero <= IsZero (kzero x)
  ? zero = kzero x : Nat
  . zero = zero : Nat
-}


fun pzero : (<zero : Nat> -> Nat) -> Nat -> <zero : Nat>
{ pzero f zero     = zero
; pzero f (succ x) = kzero (f (pzero f x)) 
}
{- type checking the second clause succees with bidirectional t.c.
   Gamma = f : <zero> -> Nat
           pzero f : Nat -> <zero>
           x : Nat

   ? Gamma |- f (pzero f x) : Nat
   ? Gamma |- pzero f x : <zero>
 -}

fun qzero : ((n : Nat) -> IsZero n -> Nat) -> Nat -> <zero : Nat>
{ qzero f zero     = zero
; qzero f (succ x) = kzero (f (qzero f x) isZero) 
}
{- type checking the second clause FAILS with bidirectional t.c.
   Gamma = f       : (n : Nat) -> IsZero n -> Nat
           qzero f : Nat -> <zero>
           x       : Nat

   ? Gamma |- f (qzero f x) isZero : Nat
     ?1 Gamma |- qzero f x : Nat
     ?2 Gamma |- isZero : IsZero (qzero f x) 

  Here we fail, since we just substituted the value (qzero f x) for n.
  The information qzero f x = 0 is lost.

One solution here world be to use the eta-expanded form of qzero also
when checking the body of qzero.  -}

-- simplified example

data Unit  : Set { unit : Unit }
data Empty : Set {}

fun Zero : (n : Nat) -> Set
{ Zero zero = Unit
; Zero (succ x) = Empty
}

let bla : ((n : Nat) -> Zero n -> Nat) -> (Nat -> <zero : Nat>) -> Nat -> Nat
        = \ f -> \ g -> \ x -> f (g x) unit
-- THIS DOES NOT DO the job, since g x is eta-expanded to zero.
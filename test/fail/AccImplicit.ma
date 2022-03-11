data Acc ( A : Set) (Lt : A -> A -> Set) *(b : A) : Set
{ acc :  (accOut : (a : A) -> Lt a b -> Acc A Lt a) -> Acc A Lt b
}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

{- R (S x) x  if x < 2
 -}
data R : Nat -> Nat -> Set
{ r1 : (x : Nat) -> R (succ (succ x)) (succ zero)
; r2 : R (succ zero) zero
}

{-
fun succR : (n : Nat) -> R (succ n) n
{ succR zero = r2
; succR (succ n) =
-}

let acc2 : (n : Nat) -> Acc Nat R (succ (succ n))
  = \ n -> acc (\ a -> \ p -> case p {})

fun aux1 : (a : Nat) -> (p : R a (succ zero)) -> Acc Nat R a
{ aux1 (succ (succ x)) (r1 .x) = acc2 x
}

let acc1 : Acc Nat R (succ zero)
  = acc aux1

fun aux0 : (a : Nat) -> (p : R a zero) -> Acc Nat R a
{ aux0 .(succ zero) r2 = acc1
}

let acc0 : Acc Nat R zero
  = acc aux0

fun accR : (n : Nat) -> Acc Nat R n
{ accR zero = acc0
; accR (succ zero) = acc1
; accR (succ (succ n)) = acc2 n
}

fun acc_dest : [n : Nat] -> (p : Acc Nat R n) ->
               (m : Nat) -> R m n -> Acc Nat R m
{ acc_dest n (acc p) = p
}

fun f : (x : Nat) -> Acc Nat R x -> Nat
{ f x (acc p) = case x
  { zero -> f (succ x) (p (succ x) r2)
  ; (succ zero) -> f (succ x) (p (succ x) (r1 zero))
  ; (succ (succ y)) -> zero
  }
}

{-
-- In Coq, g and h are accepted by the termination checker
fun g : (x : Nat) -> [Acc Nat R x] -> Nat
{ g x p = case x
  { zero -> g (succ x) (acc_dest zero p (succ x) r2)
  ; (succ zero) -> g (succ x) (acc_dest (succ zero) p (succ x) (r1 zero))
  ; (succ (succ y)) -> zero
  }
}

fun h : (x : Nat) -> [Acc Nat R x] -> Nat
{ h zero p = h (succ zero) (acc_dest zero p (succ zero) r2)
; h (succ zero) p = h (succ (succ zero)) (acc_dest (succ zero) p (succ (succ zero)) (r1 zero))
; h (succ (succ y)) p = zero
}
-}

fun h : (x : Nat) -> [Acc Nat R x] -> Nat
{ h zero (acc p) = h (succ zero) (p (succ zero) r2)
; h (succ zero) (acc p) = h (succ (succ zero)) (p (succ (succ zero)) (r1 zero))
; h (succ (succ y)) p = zero
}
{- The definition of h should be fine since

   q : Acc Nat R zero   iff  q = acc .Nat .R zero p

so the forced match does not refine the type [Acc Nat R x] further.
This means that h can be translated to case trees without any case on q,
it just uses the destructor. -}

eval let bla : Nat
  = f zero acc0

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

let p1 : (p : Acc Nat R zero) -> Id Nat (h zero p) (h zero acc0)
       = \ p -> refl
{- In a case tree representation of h this would type check! -}

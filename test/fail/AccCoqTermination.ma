{-
-- to debug make test/fail
fun f : (A : Set) -> A
{ f A = f A
}
-}

data Acc ( A : Set) ( Lt : A -> A -> Set) : A -> Set
{
  acc :  (b : A) ->
        ((a : A) -> Lt a b -> Acc A Lt a)
        -> Acc A Lt b
}

data Nat : Set
{
        zero : Nat ;
        succ : Nat -> Nat
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
  = \ n -> acc (succ (succ n)) (\ a -> \ p -> case p {})

fun aux1 : (a : Nat) -> (p : R a (succ zero)) -> Acc Nat R a
{ aux1 (succ (succ x)) (r1 .x) = acc2 x
}
-- 2010-09-20 here I would like to have internally
-- externally, there should be no dot patterns!
-- aux1 (.succ (.succ x)) (r1 .x) = acc2 x

let acc1 : Acc Nat R (succ zero)
  = acc (succ zero) aux1

fun aux0 : (a : Nat) -> (p : R a zero) -> Acc Nat R a
{ aux0 .(succ zero) r2 = acc1
}

let acc0 : Acc Nat R zero
  = acc zero aux0

fun accR : (n : Nat) -> Acc Nat R n
{ accR zero = acc0
; accR (succ zero) = acc1
; accR (succ (succ n)) = acc2 n
}

fun acc_dest : (n : Nat) -> (p : Acc Nat R n) ->
               (m : Nat) -> R m n -> Acc Nat R m
{ acc_dest .n (acc n p) = p
}

fun f : (x : Nat) -> Acc Nat R x -> Nat
{ f x (acc .x p) = case x
  { zero -> f (succ x) (p (succ x) r2)
  ; (succ zero) -> f (succ x) (p (succ x) (r1 zero))
  ; (succ (succ y)) -> zero
  }
}

-- In Coq, g and h are accepted by the termination checker
fun g : (x : Nat) -> [Acc Nat R x] -> Nat
{ g x p = case x
  { zero -> g (succ x) (acc_dest zero p (succ x) r2)
  ; (succ zero) -> g (succ x) (acc_dest (succ zero) p (succ x) (r1 zero))
  ; (succ (succ y)) -> zero
  }
}
-- MiniAgda refuses g and h

fun h : (x : Nat) -> [Acc Nat R x] -> Nat
{ h zero p = h (succ zero) (acc_dest zero p (succ zero) r2)
; h (succ zero) p = h (succ (succ zero)) (acc_dest (succ zero) p (succ (succ zero)) (r1 zero))
; h (succ (succ y)) p = zero
}

eval let bla : Nat
  = f zero acc0

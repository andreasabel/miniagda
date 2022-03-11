-- 2010-01-22 bug noted
-- 2010-07-08 bug fixed
-- 2012-01-22 parameters gone from constructors

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

-- ERROR: data AccPar [A : Set](Lt : A -> A -> Set)(b : A) : Set
data Acc (A : Set) (Lt : A -> A -> Set) *(b : A) : Set
{ acc :  (accParOut : (a : A) -> Lt a b -> Acc A Lt a) -> Acc A Lt b
}

{- 2011-04-23 does not work due to new polarities
data AccOk (A : Set)(Lt : A -> A -> Set) : A -> Set
{ accOk :  [b : A] -> (accOkOut : (a : A) -> Lt a b -> AccOk A Lt a) -> AccOk A Lt b
}
-- WAS: BUG
-- destructor generation does not work if indices are not erased
data Acc (A : Set) (Lt : A -> A -> Set) : A -> Set
{ acc :  (b : A) -> (accOut : (a : A) -> Lt a b -> Acc A Lt a) -> Acc A Lt b
}
-}

fun acc_dest : (n : Nat) -> (p : Acc Nat R n) ->
               (m : Nat) -> R m n -> Acc Nat R m
{ acc_dest n (acc p) = p
}

{-
fun succR : (n : Nat) -> R (succ n) n
{ succR zero = r2
; succR (succ n) =
-}

let acc2 : (n : Nat) -> Acc Nat R (succ (succ n))
  = \ n -> acc -- Nat R (succ (succ n))
             (\ a -> \ p -> case p {})

fun aux1 : (a : Nat) -> (p : R a (succ zero)) -> Acc Nat R a
{ aux1 (succ (succ x)) (r1 .x) = acc2 x
}

let acc1 : Acc Nat R (succ zero)
  = acc -- Nat R (succ zero)
        aux1

fun aux0 : (a : Nat) -> (p : R a zero) -> Acc Nat R a
{ aux0 .(succ zero) r2 = acc1
}

eval let acc0 : Acc Nat R zero
  = acc -- Nat R zero
        aux0

fun accR : (n : Nat) -> Acc Nat R n
{ accR zero = acc0
; accR (succ zero) = acc1
; accR (succ (succ n)) = acc2 n
}

fun f : (x : Nat) -> Acc Nat R x -> Nat
{ f x (acc {-.Nat .R .x-} p) = case x
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

-- h needs to be rejected, Acc cannot be erased at compile-time!
fun h : (x : Nat) -> [Acc Nat R x] -> Nat
{ h zero (acc {-.Nat .R .zero-} p) = h (succ zero) (p (succ zero) r2)
; h (succ zero) (acc {-.Nat .R .(succ zero)-} p) = h (succ (succ zero)) (p (succ (succ zero)) (r1 zero))
; h (succ (succ y)) p = zero
}

eval let bla : Nat
  = h zero acc0

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

-- fails, since (h zero p) does not reduce, but (h zero acc0) --> zero
fail let p1 : (p : Acc Nat R zero) -> Id Nat (h zero p) (h zero acc0)
            = \ p -> refl -- Nat (h zero acc0)

-- 2013-04-08 Dotted constructors

data Unit { unit }

data Nat { zero ; suc (n : Nat) }

fun plus : Nat -> Nat -> Nat
{ plus zero    m = m
; plus (suc n) m = suc (plus n m)
}

data List ++(A : Set) { nil ; cons (x : A) (xs : List A) }

data Vec ++(A : Set) (n : Nat)
{ vnil                                : Vec A zero
; vcons (vhead : A) (vtail : Vec A n) : Vec A (suc n)
} fields vhead, vtail

fun append : [A : Set] [n : Nat] [m : Nat] -> Vec A n -> Vec A m -> Vec A (plus n m)
{ append A .zero    m vnil         ys = ys
; append A (.suc n) m (vcons x xs) ys = vcons x (append A n m xs ys)
}

data Fin (n : Nat)
{ fzero            : Fin (suc n)
; fsuc (i : Fin n) : Fin (suc n)
}

fun lookup : [A : Set] [n : Nat] (i : Fin n) (xs : Vec A n) -> A
{ lookup A .zero    ()       vnil
; lookup A (.suc n) fzero    (vcons x xs) = x
; lookup A (.suc n) (fsuc i) (vcons x xs) = lookup A n i xs
}

{- untyped terms

data Tm (n : Nat)
{ var (x    : Fin n)
; app (r, s : Tm n)
; abs (t    : Tm (suc n))
}

let Subst (n, m : Nat) = Vec (Tm m) n

fun liftSubst : (n : Nat) [m : Nat] -> Subst n m -> Subst (suc n) (suc m)
{}

fun subst : (n : Nat) [m : Nat] -> Tm n -> Subst n m -> Tm m
{ subst n m (var i)   rho = lookup (Tm m) n i rho
; subst n m (app r s) rho = app (subst n m r rho) (subst n m s rho)
; subst n m (abs t)   rho = abs (subst (suc n) (suc m) t (liftSubst n m rho))
}
-}

data Ty { nat ; arr (a, b : Ty) }

let Cxt = List Ty

data Var (cxt : Cxt) (a : Ty)
{ vzero                 : Var (cons a cxt) a
; vsuc  (x : Var cxt b) : Var (cons a cxt) b
}

data Tm (cxt : Cxt) (a : Ty)
{ var (x : Var cxt a)                                : Tm cxt a
; app [a : Ty] (r : Tm cxt (arr a b)) (s : Tm cxt a) : Tm cxt b
; abs (t : Tm (cons a cxt) b)                        : Tm cxt (arr a b)
}

fun Sem : Ty -> Set
{ Sem nat       = Nat
; Sem (arr a b) = Sem a -> Sem b
}

fun Env : Cxt -> Set
{ Env nil         = Unit
; Env (cons a as) = Sem a & Env as
}

fun val : [cxt : Cxt] [a : Ty] -> Var cxt a -> Env cxt -> Sem a
{ val (.cons a cxt) .a vzero   (v, vs) = v
; val (.cons a cxt) b (vsuc x) (v, vs) = val cxt b x vs
}

fun sem : [cxt : Cxt] [a : Ty] -> Tm cxt a -> Env cxt -> Sem a
{ sem cxt a          (var x)     rho = val cxt a x rho
; sem cxt b          (app a r s) rho = sem cxt (arr a b) r rho (sem cxt a s rho)
; sem cxt (.arr a b) (abs t)     rho v = sem (cons a cxt) b t (v, rho)
}

{- How to check a data constructor

Case 1: no target given, e.g.

    cons (x : A) (xs : List A)

  Bring the parameters of the data telescope into scope, then
  check constructor telescope

Case 2: target given, e.g.

    vcons (vhead : A) (vtail : Vec A n) : Vec A (suc n)

  Take the parameters off the target, treat them like patterns,
  and check them against the data telecope (or type of data name).
  We get out a context

    A : Set
    n : Nat

  use this context to check full type of constructor.
  Also, check that no binding in constructor type shadows the
  pattern variables of the target (would be confusing).
  In the end, prepend the context to the constructor type.

Case 3: target is function type.

  Extract final target and proceed as in 2.

-}

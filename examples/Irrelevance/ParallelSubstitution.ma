-- 2013-04-08

-- Prelude.

data Unit { unit }
data Nat  { zero ; suc (n : Nat) }

data List ++(A : Set) { nil ; cons (x : A) (xs : List A) }

-- Types and contexts.

data Ty { nat ; arr (a, b : Ty) }

let Cxt = List Ty

-- Typed terms.

data Var +(i : Size) (cxt : Cxt) (a : Ty)
{ vzero                           : Var i (cons a cxt) a
; vsuc  [j < i] (x : Var j cxt b) : Var i (cons a cxt) b
}

data Tm +(i : Size) (cxt : Cxt) (a : Ty)

{ var (x : Var # cxt a)         : Tm i cxt a

; app [j < i] [a : Ty]
      (r : Tm j cxt (arr a b))
      (s : Tm j cxt a)          : Tm i cxt b

; abs [j < i]
      (t : Tm j (cons a cxt) b) : Tm i cxt (arr a b)
}

-- Workaround for size #.

let abs' [i : Size] [cxt : Cxt] [a, b : Ty]
    (t : Tm i (cons a cxt) b) : Tm $i cxt (arr a b)
  = abs i t

let app' [i : Size] [cxt : Cxt] [a, b : Ty]
    (r : Tm i cxt (arr a b))
    (s : Tm i cxt a)         : Tm $i cxt b
  = app i a r s

-- Semantics.

fun Sem : Ty -> Set
{ Sem nat       = Nat
; Sem (arr a b) = Sem a -> Sem b
}

fun Env : Cxt -> Set
{ Env nil         = Unit
; Env (cons a as) = Sem a & Env as
}

fun val : [i : Size] |i| [cxt : Cxt] [a : Ty] -> Var i cxt a -> Env cxt -> Sem a
{ val i (.cons a cxt) .a vzero     (v, vs) = v
; val i (.cons a cxt) b (vsuc j x) (v, vs) = val j cxt b x vs
}

fun sem : [i : Size] |i| [cxt : Cxt] [a : Ty] -> Tm i cxt a -> Env cxt -> Sem a
{ sem i cxt a          (var x)       rho = val # cxt a x rho
; sem i cxt b          (app j a r s) rho = sem j cxt (arr a b) r rho (sem j cxt a s rho)
; sem i cxt (.arr a b) (abs j t)     rho v = sem j (cons a cxt) b t (v, rho)
}

-- Parallel substitution.

data VT : Size -> Set { vr : VT 0 ; tm : VT 1 }

fun VarTm : [i : Size] |i| -> VT i -> Cxt -> Ty -> Set
{ VarTm .0 vr = Var #
; VarTm .1 tm = Tm  #
}

let RenSub [i : Size] (j : Size) (vt : VT i) (gamma, delta : Cxt)
  = [a : Ty] -> Var j delta a -> VarTm i vt gamma a

mutual {

  fun lift : [i : Size] |i,0| (vt : VT i) [gamma, delta : Cxt] [a : Ty]
             (tau : RenSub i # vt gamma delta) ->
             RenSub i # vt (cons a gamma) (cons a delta)
  -- lifting a renaming
  { lift .0 vr gamma delta a tau .a vzero     = vzero
  ; lift .0 vr gamma delta a tau b (vsuc k x) = vsuc # (tau b x)
  -- lifting a substitution
  ; lift .1 tm gamma delta a tau .a vzero     = var vzero
  ; lift .1 tm gamma delta a tau b (vsuc k x) =
      subst 0 # vr (cons a gamma) gamma
        (\ a y -> vsuc # y)
        b (tau b x)
  }

  fun subst : [i, j : Size] |i,j+1| (vt : VT i) [gamma, delta : Cxt]
              (tau : RenSub i # vt gamma delta)  [a : Ty]
              (t : Tm j delta a) -> Tm # gamma a
  { subst i j vt gamma delta tau (.arr a b) (abs k t) =
      abs' # gamma a b
        (subst i k vt (cons a gamma) (cons a delta)
           (lift i vt gamma delta a tau)
           b t)
  ; subst i j vt gamma delta tau b (app k a r s) =
      app' # gamma a b
        (subst i k vt gamma delta tau (arr a b) r)
        (subst i k vt gamma delta tau a s)
  ; subst .0 j vr gamma delta tau a (var x) = var (tau a x)
  ; subst .1 j tm gamma delta tau a (var x) = tau a x
  }
}

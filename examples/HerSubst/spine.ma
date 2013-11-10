data Nat { zero; suc (n : Nat) }

data Fin (n : Nat)
{ zero             : Fin (suc n)
; suc  (i : Fin n) : Fin (suc n)
}

data Ty { base ; arr (a, b : Ty) }

data List ++(A : Set) { nil; cons (x : A) (xs : List A) }

let Cxt = List Ty

data Var (gamma : Cxt) (a : Ty)
{ zero                   : Var (cons a gamma) a
; suc  (x : Var gamma a) : Var (cons b gamma) a
}

mutual {  -- mutual and 'parametric' polarity assignment broken?

  data Spine ^(gamma : Cxt) ^(a, c : Ty)
  { nil                                          : Spine gamma c         c
  ; cons (v : Nf gamma a) (vs : Spine gamma b c) : Spine gamma (arr a b) c
  }

  data Nf ^(gamma : Cxt) ^(a : Ty)
  { ne  (a : Ty) (x : Var gamma a) (vs : Spine gamma a c) : Nf gamma c
  ; lam (t : Nf (cons a gamma) b)                         : Nf gamma (arr a b)
  }

}

fun append : {gamma : Cxt} {a, b : Ty} (us : Spine gamma a b)
             {c : Ty}                  (vs : Spine gamma b c) -> Spine gamma a c
{ append nil         vs = vs
; append (cons u us) vs = cons u (append us vs)
}

{- TODO

mutual {

  fun apply : ([gamma : Cxt] [a : Ty] u  : Nf gamma a)
              (              [c : Ty] es : Spine gamma a c) -> Nf gamma c
  { apply b         (ne a x vs)     us  = ne a x (append vs us)
  ; apply (arr a b) (lam v)        nil  = lam v
  ; apply (arr a b) (lam v) (cons u us) = apply b (hersubst1 a v u) us
  }

  fun hersubst1 : [gamma : Cxt] [a, b : Ty] (v : Nf (cons a gamma) b) (u : Nf gamma a) -> Nf gamma b
  {}
}

-}

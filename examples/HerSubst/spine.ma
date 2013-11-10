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
  { eps                                          : Spine gamma c         c
  ; appl (v : Nf gamma a) (vs : Spine gamma b c) : Spine gamma (arr a b) c
  }

  data Nf ^(gamma : Cxt) ^(a : Ty)
  { ne  (a : Ty) (x : Var gamma a) (vs : Spine gamma a c) : Nf gamma c
  ; lam (t : Nf (cons a gamma) b)                         : Nf gamma (arr a b)
  }

}

fun append : [gamma : Cxt] [a, b, c : Ty] (us : Spine gamma a b) (vs : Spine gamma b c) -> Spine gamma a c
{ append gamma a           b c eps         vs = vs
; append gamma (arr a1 a2) b c (appl u us) vs = appl u (append gamma a2 b c us vs)
}

mutual {

  fun apply : [gamma : Cxt] [a, c : Ty] (u : Nf gamma a) (es : Spine gamma a c) -> Nf gamma c
  { apply gamma b         c (ne a x vs)     us  = ne a x (append gamma a b c vs us)
  ; apply gamma (arr a b) c (lam v)        eps  = lam v
  ; apply gamma (arr a b) c (lam v) (appl u us) = apply gamma b c (hersubst1 gamma a b v u) us
  }

  fun hersubst1 : [gamma : Cxt] [a, b : Ty] (v : Nf (cons a gamma) b) (u : Nf gamma a) -> Nf gamma b
  {}
}

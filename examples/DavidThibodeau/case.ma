data Nat : Set 
{ zero : Nat
; suc  : Nat -> Nat
}

data Maybe (A : Set) : Set
{ nothing : Maybe A
; just    : A -> Maybe A
}

fun isEven : Nat -> Maybe Nat
{ isEven zero = just zero
; isEven (suc zero) = nothing
; isEven (suc (suc n)) = case isEven n
  { (nothing) -> nothing
  ; (just m)  -> just (suc m)
  }
}
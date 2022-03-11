data Bool : Set
{ true  : Bool
; false : Bool
}

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

data List : Set
{ nil  : List
; cons : Nat -> List -> List
}

fun leq : Nat -> Nat -> Bool {}

-- merge as would be represented with "with" in Agda
mutual {
  fun merge : List -> List -> List
  { merge nil l = l
  ; merge l nil = l
  ; merge (cons x xs) (cons y ys) = merge_aux x xs y ys (leq x y)
  }
  fun merge_aux : Nat -> List -> Nat -> List -> Bool -> List
  { merge_aux x xs y ys true  = cons x (merge xs (cons y ys))
  ; merge_aux x xs y ys false = cons y (merge (cons x xs) ys)
  }
}

{- this is not recognized terminating since

  cons y ys  is in no relation with y or ys

its size is max(y,ys) + 1, but we do not honor max in termination checking
-}

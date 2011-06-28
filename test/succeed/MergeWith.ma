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
  ; merge (cons x xs) (cons y ys) = merge_aux x xs (cons x xs) y ys (cons y ys) (leq x y)
  }
  fun merge_aux : Nat -> List -> List -> Nat -> List -> List -> Bool -> List
  { merge_aux x xs xxs y ys yys true  = cons x (merge xs yys)
  ; merge_aux x xs xxs y ys yys false = cons y (merge xxs ys) 
  }
}
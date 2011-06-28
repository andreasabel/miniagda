data Bool : Set
{ true  : Bool
; false : Bool
}

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

sized data List : Size -> Set
{ nil  : (i : Size) -> List ($ i)  
; cons : (i : Size) -> Nat -> List i -> List ($ i)
}

fun leq : Nat -> Nat -> Bool {}

-- merge as would be represented with "with" in Agda
mutual {
  fun merge : (i : Size) -> List i -> (j : Size) -> List j -> List #
  { merge .($ i) (nil i) j l = l
  ; merge i l .($ j) (nil j) = l
  ; merge .($ i) (cons i x xs) .($ j) (cons j y ys) = merge_aux i x xs j y ys (leq x y)
  }
  fun merge_aux : (i : Size) -> Nat -> List i -> (j : Size) -> Nat -> List j -> Bool -> List #
  { merge_aux i x xs j y ys true  = cons # x (merge i xs ($ j) (cons j y ys))
  ; merge_aux i x xs j y ys false = cons # y (merge ($ i) (cons i x xs) j ys) 
  }
}
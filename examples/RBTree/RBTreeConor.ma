data One : Set
{ unit : One
}

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

fun plus : Nat -> Nat -> Nat
{ plus zero    m = m
; plus (suc n) m = plus n m
}

data Color : Set 
{ red   : Color
; black : Color
}

data Key : Set {}

let TreeType : Set 1 = Color -> Nat -> Set

fun RBF : ++TreeType -> TreeType
{ RBF X red    n      = X black n & Key & X black n
; RBF X black  zero   = One
; RBF X black (suc n) = (c : Color) & X c n & Key & X c n
}

cofun RBT : ++(i : Size) -> (c : Color) -> (n : Nat) -> Set
{ RBT i c n = (j < i) & RBF (RBT j) c n
}

fun size : [i : Size] -> (c : Color) -> (n : Nat) -> RBT i c n -> Nat
{ size i red n (j < i , l , k , r) = 
   suc (plus (size j black n l) (size j black n r))
; size i black zero t = zero
; size i black (suc n) (j < i , c , l , k , r) =
   suc (plus (size j c n l) (size j c n r))
}



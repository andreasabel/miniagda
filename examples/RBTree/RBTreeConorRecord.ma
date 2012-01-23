data One : Set
{ unit : One
}

data Sig ++(A : Set) ++(B : A -> Set) : Set
{ pair : (fst : A) -> (snd : B fst) -> Sig A B
} fields fst, snd

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

data RedNode ++(X : TreeType) *(n : Nat) : Set
{ redNode : 
    (redLeft  : X black n) ->
    (redKey   : Key) ->
    (redRight : X black n) -> RedNode X n
} fields redLeft, redKey, redRight

data BlackNode ++(X : TreeType) *(n : Nat) : Set
{ blackNode : 
    (color : Color) ->
    (blackLeft : X color n) ->
    (blackKey  : Key) ->
    (blackRight : X color n) ->
    BlackNode X n
} fields color, blackLeft, blackKey, blackRight

fun RBF : ++TreeType -> TreeType
{ RBF X red n = RedNode X n
; RBF X black zero = One
; RBF X black (suc n) = BlackNode X n
}

data RBT *(c : Color) *(n : Nat) : Set
{ tree : RBF RBT c n -> RBT c n
}

{- 2012-01-23 EXTRACTION ERROR
fun size : (c : Color) -> (n : Nat) -> RBT c n -> Nat
{ size red n (tree (redNode l k r)) = 
   suc (plus (size black n l) (size black n r))
; size black zero t = zero
; size black (suc n) (tree (blackNode c l k r)) =
   suc (plus (size c n l) (size c n r))
}
-}


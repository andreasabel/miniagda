-- 2011-04-20

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

fun pred : Nat -> Nat
{ pred zero    = zero
; pred (suc n) = n
}

let kzero : .Nat -> Nat
  = \ n -> zero

fun f : ^(*Nat -> Nat) -> Nat
{ f g = g zero
}

fun f'2 : .(*Nat -> Nat) -> Nat
{ f'2 g = pred (kzero (g zero)) --> pred zero --> zero
}

{- ERROR g not allowed under pred
fun f' : ^(*Nat -> Nat) -> Nat
{ f' g = pred (g zero)
}
-}

{-
fun f1 : ^(*Nat -> Nat) -> ^(*Nat -> Nat) -> Nat
{ f1 g k = k (g zero)
}
-}

data Bool : Set
{ true  : Bool
; false : Bool
}

fun T : Nat -> Set
{ T zero    = Nat
; T (suc n) = Bool
}

fun f3 : [X : Nat -> Set] -> X zero -> Nat
{ f3 X x = zero
}

-- the following fails because we end up comparing a Nat to a Bool
fail
fun bad :
 [F : [X : Nat -> Set] -> X zero -> Set] ->
 (g : F T zero)                          ->
 (h : F (\ n -> T (suc n)) true -> Bool) -> Bool
{ bad F g h = h g
}

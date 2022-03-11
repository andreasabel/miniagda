data Bool : Set
{ true  : Bool
; false : Bool
}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun T : Bool -> Set
{ T true  = Nat
; T false = Bool
}

-- type checking fails with message "zero != false"
-- can be harmful if constructors can be reused in different types
fail
fun bad :
  [F : Nat -> Set] ->
  [f : [x : Bool] -> T x -> Nat] ->
  (g : (n : Nat) -> F (f true n)) ->
  (h : F (f false false) -> Bool) ->
  Bool
{ bad F f g h = h (g zero)
}

{- h  expects  _ : F (f false false)
   but    g zero : F (f true  zero)

?  F (f true zero) <= F (f false false)
?  f true zero : Nat = f false false : Nat
?  zero : (T x)[true/x] = false : (T x)[false/x]
?  zero : Nat = false : Bool

should abort with message Nat != Bool
-}

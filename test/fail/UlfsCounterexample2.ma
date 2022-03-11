data Bool : Set
{ true  : Bool
; false : Bool
}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data T : Bool -> Set
{ nat  : Nat  -> T true
; bool : Bool -> T false
}

-- type checking fails with message "nat != bool"
-- can be harmful if constructors can be reused in different types
fun bad :
  [F : Nat -> Set] ->
  ^(f : [x : Bool] -> T x -> Nat) ->
  (g : (n : Nat) -> F (f true (nat n))) ->
  (h : F (f false (bool false)) -> Bool) ->
  Bool
{ bad F f g h = h (g zero)
}
-- 2010-10-01 now it is checked before that
-- nat and bool are in the same family T

-- 2010-10-01

-- an example with different types in context during eq. checking
-- derived from Ulf's counterexample

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

fun good : 
  [F : Nat -> Set] ->
  [f : [b : Bool] -> ([T b] -> Nat) -> Nat] ->
  (g : (n : Nat) -> F (f true (\ x -> n))) ->
  (h : F (f false (\ x -> zero)) -> Bool) -> 
  Bool
{ good F f g h = h (g zero)
}

let good' : 
    [F : [b : Bool] -> ([T b] -> Nat) -> Set] ->
    (g : F false (\ x -> zero) -> Bool) -> 
    (h : (n : Nat) -> F true (\ x -> n)) ->
    Bool
  = \ F g h -> g (h zero)



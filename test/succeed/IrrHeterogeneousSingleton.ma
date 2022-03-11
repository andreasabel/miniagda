-- 2010-10-01
-- Should heterogeneous equality x : <a : A> ?= a : A
-- succeed?  I'd say yes!

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
; T false = <zero : Nat>
}

fun good :
  [F : Nat -> Set] ->
  [f : [x : Bool] -> T x -> Nat] ->
  (z : T false) ->
  (g : (n : Nat) -> F (f true n)) ->
  (h : F (f false z) -> Bool) ->
  Bool
{ good F f z g h = h (g zero)
}

{- f true zero ?= f false z : Nat
   zero : Nat  ?= z : <zero : Nat>
-}


-- 2010-10-09

-- an example with different types in context during eq. checking
-- derived from Ulf's counterexample

data Unit : Set
{ unit : Unit
}

data Bool : Set
{ true  : Bool
; false : Bool
}

fun T : Bool -> Set
{ T true  = Bool -> Bool
; T false = Bool
}

-- fails with "Bool -> Bool has different shape than Bool"
fail
let etaFun' : 
    [F : [b : Bool] -> (T b -> T b) -> Bool -> Set] ->
    (g : F false (\ x -> x) true -> Bool) -> 
    (h : (a : Bool) -> F true (\ x y -> x y) a) ->
    Bool
  = \ F g h -> g (h true)
-- but succeeds in ICC

{- compares (cannot eta-expand lhs!)

    F false (\ x -> x) true ?= F true (\ x y -> x y) true
    x : Bool |- x : Bool    ?= x : Bool -> Bool |- \ y -> x y : Bool -> Bool

-}

fail
let etaFun : 
    [F : [b : Bool] -> (T b -> T b) -> Set] ->
    (g : F false (\ x -> x) -> Bool) -> 
    (a : F true (\ x y -> x y)) ->
    Bool
  = \ F g a -> g a

{- compares (cannot eta-expand lhs!)

    F false (\ x -> x) true ?= F true (\ x y -> x y) true
    x : Bool |- x : Bool    ?= x : Bool -> Bool |- \ y -> x y : Bool -> Bool

  works with eta-contraction, but...
-}

fun U : Bool -> Set
{ U true  = Unit
; U false = Bool
}

fail
let etaUnit' : 
    [F : [b : Bool] -> (U b -> U b) -> Bool -> Set] ->
    (g : F false (\ x -> x) true -> Bool) -> 
    (h : (a : Bool) -> F true (\ x -> unit) a) ->
    Bool
  = \ F g h -> g (h true)

{- 
    F false (\ x -> x) true ?= F true (\ x -> unit) true
    x : Bool |- x : Bool    ?= x : Unit |- unit : Unit
-}

let etaUnit : 
    [F : [b : Bool] -> (U b -> U b) -> Set] ->
    (g : F false (\ x -> x) -> Bool) -> 
    (a : F true (\ x -> unit)) ->
    Bool
  = \ F g a -> g a

{- 
    F false (\ x -> x) true ?= F true (\ x -> unit) true
    x : Bool |- x : Bool    ?= x : Unit |- unit : Unit
-}


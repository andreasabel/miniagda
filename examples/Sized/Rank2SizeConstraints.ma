{-
module SizedTypesScopeViolationInMeta where

open import Size

data Nat : {size : Size} -> Set where
  zero : {size : Size} -> Nat {↑ size}
  suc  : {size : Size} -> Nat {size} -> Nat {↑ size}

-- exposing a bug in size constraint solving

-- *** Exception: Inconsistent meta: _73 [[CtxId 420],[CtxId 418]]
A : Set1
A = (Id : {i : Size} -> Nat {_} -> Set)
    (k : Size)(m : Nat {↑ k}) -> Id {k} m
    ->
    (j : Size)(n : Nat {j}) -> Id {j} n
-}
-- Agda issue 300

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; suc  : [i : Size] -> Nat i -> Nat $i
}

{- causes panic

fun A : Set 1
{ A =  (Id : (i : Size) -> Nat _ -> Set) ->
        (k : Size) -> (m : Nat $k) -> Id k m ->
        (j : Size) -> (n : Nat j)  -> Id j n
}
-- should solve the _ with i


fun test : (Id : (i : Size) -> Nat _ -> Set) ->
           ((k : Size) -> (m : Nat $k) -> Id k m) ->
            (j : Size) -> (n : Nat j)  -> Id j n
{ test Id cont j n = cont j n
}
-}

{- DOES NOT WORK YET
addMeta 0 scope [i]
whnf meta ?0{i = v0}
whnf meta ?0{i = v1}
Constraint: ($ v1) <= ?0{i = v1}
whnf meta ?0{i = v4}
Constraint: v4 <= ?0{i = v4}
solutionfromList [(0,[v1+1,v4])]
miniagda: panic: sizeExprToExpr (v1+1): variable v1 not in scope []
-}

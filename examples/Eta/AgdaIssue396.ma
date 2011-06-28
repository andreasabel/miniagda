{- 2011-03-23 Agda issue 396

record T : Set where
  constructor tt

foo : (P : T -> Set) ->
      ((x : T) -> P x -> P x) ->
      (x y : T) -> P x -> P y
foo P hyp x y = hyp x

-- Bug.agda:9,17-22
-- x != y of type T
-- when checking that the expression hyp x has type P x -> P y
-}

data T : Set
{ tt : T }

fun foo : (P : T -> Set) ->
          ((x : T) -> P x -> P x) ->
          (x, y : T) -> P x -> P y
{ foo P hyp x y = hyp x }
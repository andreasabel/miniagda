-- 2010-09-10  Lambda-Bool examples

{- Smart case is not transitive -}

data Bool : Set
{ true  : Bool
; false : Bool
}

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

let if : (A : Set) -> Bool -> A -> A -> A
  = \ A c d e -> case c
    { true -> d
    ; false -> e
    }

trustme -- only works with strong smart if
let ifx : (x : Bool) -> Id Bool (if Bool x x x) x
  = \ x -> refl

let eta : (x : Bool) -> Bool
  = \ x -> if Bool x true false

let not : Bool -> Bool
  = \ x -> case x
  { true -> false
  ; false -> true
  }

trustme
let notnot1 : (x : Bool) -> Id Bool (not (not x)) (eta x)
  = \ x -> refl -- Bool (eta x)

fail let notnot1' : (x : Bool) -> Id Bool (not (not x)) (eta x)
  = \ x -> refl -- Bool x

trustme
let notnot : (x : Bool) -> Id Bool (not (not x)) x
  = \ x -> refl -- Bool (eta x)

fail let notnot' : (x : Bool) -> Id Bool (not (not x)) x
  = \ x -> refl -- Bool x

{- CANNOT INFER

  not x --> true |- x = false

This means that the system sees

  x = eta x = not (not x)

but not

  x = not (not x)

thus, equality is not transitive.

Weaker rule:


  c = c' : Bool    c=true |- d = d' : C    c=false |- e = e' : C
  --------------------------------------------------------------
  if c d e = if c' d' e' : C

-}


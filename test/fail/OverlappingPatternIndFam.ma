-- 2009-09-19
-- unsound eta-expansion as noted by Anton Setzer

data Bool : Set
{ true  : Bool
; false : Bool
}

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

fun subst : (A : Set) -> (a : A) -> (b : A) -> Id A a b ->
  (P : A -> Set) -> P a -> P b
{ subst A a .a (refl) P x = x
}

-- an overlapping ind. fam.
data DecEq (A : Set)(a : A) : A -> Set
{ eq    : DecEq A a a
; notEq : (b : A) -> DecEq A a b
}

-- every function into DecEq is the constant notEq one
-- that is provable in the current implementation of eta, but is unsound
let offDiag : (A : Set) -> (f : (a : A) -> (b : A) -> DecEq A a b) ->
              (a : A) -> (b : A) ->
              Id (DecEq A a b) (f a b) (notEq A a b)
  = \ A -> \ f -> \ a -> \ b -> refl -- (DecEq A a b) (notEq A a b)

-- let incons : (A : Set) -> (a : A) -> Id (DecEq A a a) (eq A a) (notEq A a a)
--   = \ A -> \ a -> offDiag (\ A' -> \ a' -> \ b -> eq A' a') A a a

fun f : (x : Bool) -> (y : Bool) -> DecEq Bool x y
{ f true true = eq Bool true
; f true false = notEq Bool true false
; f false true = notEq Bool false true
; f false false = eq Bool false
}

-- now we can show that two constructors are equal
let incons : Id (DecEq Bool true true) (eq Bool true) (notEq Bool true true)
 = offDiag Bool f true true

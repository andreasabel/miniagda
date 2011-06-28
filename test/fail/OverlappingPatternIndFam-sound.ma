data Bool : Set
{ true  : Bool
; false : Bool
}

data Id (A : Set) (a : A) : A -> Set 
{ refl : Id A a a 
}

fun subst : (A : Set) -> (a : A) -> (b : A) -> Id A a b -> 
  (P : A -> Set) -> P a -> P b
{ subst A a .a (refl .A .a) P x = x
}

-- an overlapping ind. fam.
data DecEq (A : Set)(a : A) : A -> Set 
{ eq    : DecEq A a a
; notEq : (b : A) -> DecEq A a b
}

-- this rightfully does not type check, since f A a a does not expand to eq
-- (both patterns match)
let fDiag : (f : (A : Set) -> (a : A) -> (b : A) -> DecEq A a b) ->
             (A : Set) -> (a : A) -> Id (DecEq A a a) (f A a a) (eq A a)
  = \ f -> \ A -> \ a -> refl (DecEq A a a) (eq A a)

let incons : (A : Set) -> (a : A) -> Id (DecEq A a a) (notEq A a a) (eq A a)
  = fDiag notEq
data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

fun subst : (A : Set) -> (a : A) -> (b : A) -> Id A a b ->
  (P : A -> Set) -> P a -> P b
{ subst A a .a (refl) P x = x
}

-- this does not typecheck since f A a b expands to * but subst blocks
let offDia : (f : (A : Set) -> (a : A) -> (b : A) -> Id A a b) ->
             (A : Set) -> (a : A) -> (b : A) ->
              Id (Id A a b)
               (f A a b)
               (subst A a b (f A a b) (Id A a) (refl))
  = \ f -> \ A -> \ a -> \ b -> refl {- (Id A a b) (subst A a b (f A a b) (Id A a) (refl A a)) -}

data Id (A : Set) (a : A) : (B : Set) -> B -> Set 1
{ refl : Id A a A a
}

-- this does not typecheck since f A a B b expands to *, not to refl
let offDia : (f : (A : Set) -> (B : Set) -> (a : A) -> (b : B) -> Id A a B b) ->
             (A : Set) -> (B : Set) -> (a : A) -> (b : B) ->
              Id (Id A B a b)  (f A B a b)
                 (Id A a A a)  (refl A a)
  = \ f -> \ A -> \ B -> \ a -> \ b -> refl (Id A a A a) (refl A a)

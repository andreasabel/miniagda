-- 2011-04-23

data Eq (A : Set) (a : A) : A -> Set
{ refl : Eq A a a }

let test : (id : [A : Set] -> A -> A) ->
  [B : Set] -> (f : B -> B) -> 
  Eq (B -> B) (id (B -> B) (\ x -> f x)) 
              (id (B -> B) f)
  = \ id B f -> refl (B -> B) (id (B -> B) f)
 
{-
let id : [A : Set] -> A -> A
  = \ A a -> a
-}

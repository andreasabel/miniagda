-- 2010-10-04 extract let definitions

-- the polymorphic identity

let id : [A : Set] -> A -> A
 = \ A x -> x

let s  : [A, B, C : Set] -> (A -> B -> C) -> (A -> B) -> A -> C
  = \ A B C x y z -> x z (y z)

let k : [A, B : Set] -> A -> B -> A
  = \ A B x y -> x

let skk : [A : Set] -> A -> A
  = \ A -> s A (A -> A) A (k A (A -> A)) (k A A)

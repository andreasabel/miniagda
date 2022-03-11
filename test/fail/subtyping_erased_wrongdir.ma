-- wrong direction of subtyping
-- not every function is a function which does not use its argument

let id : [A : Set] -> (A -> A) -> ([A] -> A)
       = \ A -> \ x -> x


-- every function which does not use its argument is a function
-- this is UNSOUND under erasure!

let id : [A : Set] -> ([A] -> A) -> (A -> A)
       = \ A -> \ x -> x


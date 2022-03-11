-- invalid use of erased data

let id : (A : Set) -> [A] -> A
       = \ A -> \ x -> x


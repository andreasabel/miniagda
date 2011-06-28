-- 2010-06-19

-- A only pos, not strictly pos.

let DNeg : Set -> ++Set -> Set
         = \ B -> \ A -> (A -> B) -> B

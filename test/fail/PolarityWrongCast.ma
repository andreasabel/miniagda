-- 2010-06-19

let DNeg : Set -> +Set -> Set
         = \ B -> \ A -> (A -> B) -> B

data Empty : Set {}

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

-- hide positivity behind recursion
fun Id : Nat # -> ++Set -> Set
{ Id (zero .#)   A = A
; Id (succ .# n) A = A
}

-- SUBTYPING the wrong way round
let kast : [i : Size] -> [n : Nat i] -> Id n (Nat #) -> Id n (Nat i)
         = \ i -> \ n -> \ x -> x


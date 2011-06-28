data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

fun f : Nat -> Nat
{}

-- 2010-09-03 currently, MiniAgda parses "index" as an index
-- but it is not computable from D A (f index)
data D ++(A : Nat -> Set) : Nat -> Set
{ mkD : (index : Nat) -> (content : A index) -> D A (f index)
} 

{- generates
fun content : [A : Nat -> Set] -> (index : Nat) -> (d : D A (_f index)) -> A index
{ content A index (mkD .A .index c) = c
}
-}
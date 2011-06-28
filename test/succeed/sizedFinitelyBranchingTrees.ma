data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data Fin : Nat -> Set
{ fzero : [n : Nat] -> Fin (succ n)
; fsucc : [n : Nat] -> Fin n -> Fin (succ n)
}

sized data Tree (A : Set) : Size -> Set
{ leaf : [i : Size] -> A -> Tree A ($ i)
; node : [i : Size] -> (n : Nat) -> (Fin n -> Tree A i) -> Tree A ($ i)
}

fun map : [A : Set] -> [B : Set] -> (A -> B) -> 
          [i : Size] -> Tree A i -> Tree B i
{ map A B f i (leaf .A (i > j) a)   = leaf B j (f a)
; map A B f i (node .A (i > j) n s) = node B j n (\ k -> map A B f j (s k)) 
}
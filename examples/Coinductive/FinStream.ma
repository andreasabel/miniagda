-- 2010-07-09

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data Fin : Nat -> Set
{ fzero : [m : Nat] -> Fin (succ m)
; fsucc : [m : Nat] -> Fin m -> Fin (succ m)
}

check
codata FinCoLi : Nat -> Set
{ nil     : [m : Nat] -> FinCoLi m
; cons    : [m : Nat] -> Fin m -> FinCoLi m -> FinCoLi m
; conSucc : [m : Nat] -> Fin (succ m) -> FinCoLi m -> FinCoLi (succ m)
}

sized codata FinCoList : Size -> Nat -> Set
{ nil     : [i : Size] -> [m : Nat] -> FinCoList $i m
; cons    : [i : Size] -> [m : Nat] -> Fin m -> FinCoList i m -> FinCoList $i m
; conSucc : [i : Size] -> [m : Nat] -> Fin (succ m) -> FinCoList i m -> FinCoList $i (succ m)
}

-- Q: How to write this nicely as a Setzer-style coalg?

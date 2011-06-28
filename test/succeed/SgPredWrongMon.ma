-- 2010-06-20

let Pred : -Set -> Set 1
         = \ A -> A -> Set

data Sg ++(A : Set) : A -> Set
{ sg : (elem : A) -> Sg A elem
}

sized data Nat : +Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

let Sg' : +(A : Set) -> A -> Set
        = Sg
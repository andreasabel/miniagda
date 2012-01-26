-- proof irrelevance via polymorphism

data Sigma (A : Set) (B : A -> Set) : Set
{ pair : (fst : A) -> (snd : B fst) -> Sigma A B
}
fields fst, snd

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data Empty : Set
{
}

-- magic = abort  does not need the inhabitant p : Empty
fun magic : [A : Set] -> [p : Empty] -> A
{ 
}

data Unit : Set
{ unit : Unit
}

fun Vec : [A : Set] -> (n : Nat) -> Set
{ Vec A zero     = Empty
; Vec A (succ n) = Sigma A (\ z -> Vec A n)
}

fun Leq : (n : Nat) -> (m : Nat) -> Set
{ Leq  zero     m        =  Unit
; Leq (succ n)  zero     =  Empty
; Leq (succ n) (succ m)  =  Leq n m
}
let Lt : (n : Nat) -> (m : Nat) -> Set
       = \ n -> \ m -> Leq (succ n) m

fun lookup : [A : Set] -> (n : Nat) -> (m : Nat) -> [Lt m n] -> Vec A n -> A
{ lookup A  zero    m        p v = magic A p
; lookup A (succ n) zero     p v = fst v -- fst A (\ z -> Vec A n) v
; lookup A (succ n) (succ m) p v = lookup A n m p <| snd v -- (snd A (\ z -> Vec A n) v)
}


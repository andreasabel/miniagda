data Nat : Set
{
zero : Nat ;
succ : Nat -> Nat
}

fun o2e : Nat -> Nat 
{
o2e zero            = zero ;
o2e (succ zero)     = zero ; 
o2e (succ (succ x)) = succ (succ (o2e x)) 
}

fun crazy : Nat -> Nat -> Nat
{
crazy zero y     = y;
crazy (succ x) y = succ (crazy y (o2e x)) 
}

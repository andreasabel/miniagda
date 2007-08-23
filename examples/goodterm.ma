data Nat : Set
{
  succ : Nat -> Nat;
  zero : Nat
}

fun Id : ( Nat -> Nat) -> (Nat -> Nat)
{
Id x = x
} 

-- should termination check 
fun zz :  Nat -> Nat -> Nat  
{
zz zero y = y;
zz (succ (succ x)) y = (Id (zz x)) y;
zz (succ x) y = zz x y 
}
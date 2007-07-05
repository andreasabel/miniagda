data Nat : Set 
{
  zero : Nat ;
  succ : Nat -> Nat 
}

data Vec (A : Set ) : Set
{ 
  nil : Vec A
}
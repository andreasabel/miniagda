data Nat : Set
{
  zero : Nat;
  suc : Nat -> Nat
}

data D : Nat -> Set 
{
  abs : (D zero -> D zero) -> D zero;
  app : (n : Nat) -> D n -> D n -> D n
}
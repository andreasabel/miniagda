data Nat : Set
{
  zero : Nat;
  succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{
  add zero y = y;
  add (succ x) y = succ (add x y)
}

data Vec (+A : Set) : Nat -> Set
{
  vnil  : Vec A zero;
  vcons : A -> [n : Nat] -> Vec A n -> Vec A (succ n)
}

fun length : [A : Set] -> [n : Nat] -> Vec A n -> Nat
{
  length A .zero (vnil) = zero;
  length A .(succ n) (vcons x n xs) = succ n  -- error: erased n may not occ.
}

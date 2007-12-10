data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat 
}

fun add : Nat -> Nat -> Nat {
  add zero = \y -> y;
  add (succ x) = \y -> succ (add x y)
}

codata Stream : Nat -> Set 
{
  cons : (i : Nat ) -> Nat -> Stream i -> Stream (succ i) 
}

-- incomplete pattern
cofun zeroes : (i : Nat ) -> Stream i
{
cons (succ i) = cons i zero (zeroes i)
}

fun head : (i : Nat ) -> Stream (succ i) -> Nat
{
head .i (cons i x xl) = x
}




data Nat : Set
{
  zero : Nat;
  succ : (pred : Nat) -> Nat
}

fun add : Nat -> Nat -> Nat
{
  add zero y = y;
  add (succ x) y = succ (add x y)
}

data Vec (+A : Set) : Nat -> Set
{
  vnil  : Vec A zero;
  vcons : (head : A) -> [n : Nat] -> (tail : Vec A n) -> Vec A (succ n)  
}

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

let vec0vnil : (A : Set) -> (n : Nat) -> (v : Vec A n) -> (v' : Vec A n) ->
               Id (Vec A n) v v'
             = \ A -> \ n -> \ v -> \ v' -> refl -- (Vec A n) v

 

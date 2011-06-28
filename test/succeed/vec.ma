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

data Vec' (+A : Set) : Nat -> Set
{
  vnil'  : Vec' A zero;
  vcons' :  [n : Nat] -> (head' : A) -> (tail' : Vec' A n) -> Vec' A (succ n)  
}

data Vec (+A : Set) : Nat -> Set
{
  vnil  : Vec A zero;
  vcons : (head : A) -> [n : Nat] -> (tail : Vec A n) -> Vec A (succ n)  
}

fun length : [A : Set] -> [n : Nat] -> Vec A n -> Nat
{
  length .A .zero (vnil A) = zero;
  length .A .(succ n) (vcons A x n xs) = succ (length A n xs);
}

fun append : [A : Set] -> [n : Nat] -> Vec A n -> 
                          [m : Nat] -> Vec A m -> Vec A (add n m)
{
  append .A .zero     (vnil A)         m ys = ys;
  append .A .(succ n) (vcons A x n xs) m ys = 
    vcons A x (add n m) (append A n xs m ys)
}

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

let vec0vnil : (A : Set) -> (v : Vec A zero) -> Id (Vec A zero) v (vnil A)
             = \ A -> \ v -> refl (Vec A zero) v

 

data Nat : Set
{
  zero : Nat ;
  succ : Nat -> Nat
}

codata Colist (A : Set) : Set
{
  nil  : Colist A ;
  cons : A -> Colist A -> Colist A
}

fun length : (A : Set) -> Colist A -> Nat
{
  length .A (nil A) = zero ;
  length .A (cons A a as) = succ (length A as)
}
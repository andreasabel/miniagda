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

codata CoNat : Set
{
  cozero : CoNat;
  cosucc : CoNat -> CoNat
}

cofun length2 : ( A : Set ) -> Colist A -> CoNat
{
length .A (nil A) = cozero;
length .A (cons A a as) = cosucc (length2 A as) 
}

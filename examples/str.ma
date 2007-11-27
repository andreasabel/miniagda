data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat 
}

fun add : Nat -> Nat -> Nat {
  add zero = \y -> y;
  add (succ x) = \y -> succ (add x y)
}

codata Stream : Set 
{
  cons : Nat -> Stream -> Stream 
}

cofun ones : Stream 
{
  ones = cons zero ones
}
 
fun tail : Stream -> Stream 
{
  tail (cons n ns) = ns
}

fun head : Stream -> Nat 
{
  head (cons n ns) = n
}

fun force : Stream -> Stream
{
  force (cons n ns) = cons n ns
}

data Eq ( A: Set ) : A -> A -> Set
{
  refl : (a : A ) -> Eq A a a 
}


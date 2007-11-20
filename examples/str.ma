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

 
fun tail : Stream -> Stream 
{
  tail (cons n ns) = ns
}

fun head : Stream -> Nat 
{
  head (cons n ns) = n
}

cofun bla : Stream 
{
bla = tail (cons zero bla)
}

eval const div : Nat = head bla
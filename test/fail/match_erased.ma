data Nat : Set
{
  zero : Nat;
  succ : Nat -> Nat
}

-- The following should not type check.
fun illegal_match : Nat -> [Nat] -> Nat
{
  illegal_match x zero = x;
  illegal_match x (succ y) = x
}

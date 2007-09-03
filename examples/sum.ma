data Nat : Set
{
zero : Nat;
succ : Nat -> Nat
}

data Sum : Set
{
inl : Nat -> Sum;
inr : Nat -> Sum
}

mutual 
{

fun f : Nat -> Nat
{
f zero = zero;
f (succ x) = g (inl x)
}

fun g : Sum -> Nat
{
g (inl x) = f x;
g (inr x) = h x
}

fun h : Nat -> Nat
{
h zero = zero;
h (succ x) = g (inr x)
}
}
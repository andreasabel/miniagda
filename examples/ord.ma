data Nat : Set
{
zero : Nat;
succ : Nat -> Nat
}

fun add : Nat -> Nat -> Nat
{

add x zero = x ;
add x (succ y) = succ (add y x)

}

data Ord : Set
{

ordzero : Ord;
olim : ( Nat -> Ord ) -> Ord

}

fun addord : Ord -> Ord -> Ord
{
addord x ozero = x ;
addord x (olim f) = olim (\n -> addord x (f n))
}

fun foo : Ord -> (Nat -> Ord) -> Ord
{
foo ozero    g = ozero;
foo (olim f) g = olim (\n -> foo (g n) f)
}

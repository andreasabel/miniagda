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

-- needs axiom f n <= f
fun addord : Ord -> Ord -> Ord
{
addord x ozero = x ;
addord x (olim f) = olim (\n -> addord x (f n))
}


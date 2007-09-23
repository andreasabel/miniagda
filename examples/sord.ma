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

data Ord : Size -> Set
{
ordzero : (i : Size ) -> Ord (s i);
olim :  (i : Size ) ->  ( Nat -> Ord i ) -> Ord (s i) ; 
}

-- does NOT need axiom f n <= f 
fun addord : (i : Size ) -> (j : Size ) -> Ord i -> Ord j -> Ord infty 
{
addord i j x ozero = x ;
addord i .(s j) x (olim j f) = olim infty (\n -> addord i j x (f n))
}


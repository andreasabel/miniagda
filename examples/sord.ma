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
ozero : (i : Size ) -> Ord ($ i);
olim :  (i : Size ) ->  ( Nat -> Ord i ) -> Ord ($ i) ; 
}

-- does not need axiom f n <= f 
fun addord : Ord # -> (i : Size )-> Ord i -> Ord # 
{
addord x .($ i) (ozero i) = ozero i ;
addord x .($ i) (olim i f) = olim # (\n -> addord x i (f n))
}


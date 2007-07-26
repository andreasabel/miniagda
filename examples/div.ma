-- sized natural numbers

data SNat : Size -> Set 
{

	succ : ( i : Size ) -> SNat i -> SNat (s i);
	zero : ( i : Size ) -> SNat (s i)
}

const Nat : Set = SNat infty

const z : Nat = zero infty
const one : Nat = succ infty z
const two : Nat = succ infty one

fun weakNat : (i : Size) -> SNat i -> SNat (s i)
{

i       (zero i) = (zero (s i));
(s i)   (succ (s i) x) = succ (weakNat i (succ i x)) 

}


data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat ($ i);
	succ : (i : Size) -> SNat i -> SNat ($ i)
}

data Maybe (A : Set ) : Set
{
  nothing : Maybe A;
  just : A -> Maybe A
}

const Nat : Set = SNat #

fun shift_case : (i : Size) -> Maybe (SNat ($ i)) -> Maybe (SNat i)
{

shift_case i (nothing .(SNat ($ i))) = nothing (SNat i);
shift_case .i (just .(SNat ($ i)) (zero i)) = nothing (SNat i);
shift_case .i (just .(SNat ($ i)) (succ i x)) = just (SNat i) x

}

const shift : (i : Size) -> (Nat -> Maybe (SNat ($ i))) -> Nat -> Maybe (SNat i) = 
\i -> \f -> \n -> shift_case i (f (succ # n))

const inc : Nat -> Maybe Nat = \n -> just Nat (succ # n)

data Unit : Set
{
	unit : Unit
}

data Bla ( u : Unit ) : Set
{
bla : Bla u
}

mutual 
{

fun boo : Unit -> (loop # (zero #) inc Unit) 
{
boo x = bla x
} 

fun loop : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Set -> Set
{
loop .($ i) (zero i) f s = loop_case ($ i) (zero i) f (f (zero i)) s ;
loop .($ i) (succ i n) f s = loop i n (shift i f) s
}

fun loop_case : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> Set -> Set
{
loop_case i       x f (nothing .(SNat i)) s = s;
loop_case .($ i)  x f (just .(SNat ($ i))  (zero i)) s = s;
loop_case .($ i)  x f (just .(SNat ($ i)) (succ i y)) s = loop i y (shift i f) s 
}
}

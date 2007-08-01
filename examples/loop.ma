data SNat : Set
{
	zero : (i : Size ) -> SNat (s i);
	succ : (i : Size) -> SNat i -> SNat (s i)
}

data Maybe (A : Set) : Set
{
	nothing : Maybe A;
	just : A -> Maybe A
}

const Nat : Set = SNat infty

fun shift_case : Maybe Nat -> Maybe Nat
{
shift_case nothing = nothing ;
shift_case (just (zero a)) = nothing ;
shift_case (just (succ a x)) = just x
}

fun shift : (i : Size) -> (Nat -> Maybe (SNat (succ i))) -> Nat -> Maybe (SNat i)
{

shift i f n = shift_case (f (succ (s i) n))

}

fun inc : Nat -> Maybe Nat
{


inc n = just (succ infty n)

}

data Unit : Set
{
	unit : Unit
}

mutual 
{

fun loop_case : (i : Size ) -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> Unit
{

loop_case i f nothing = unit;
loop_case i f (just (zero a)) = unit;
loop_case i f (just (succ a y)) = loop i y (shift i f) 

}

fun loop : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
{

loop (s i) (zero a) f = loop_case i f (f (zero a));
loop (s i) (succ a n) f = loop i n (\(x:Nat) -> shift i f x)

}

}

const diverge : Unit = loop infty (zero infty) inc
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
	nothing = nothing ;
	(just (zero a)) = nothing ;
	(just (succ a x)) = just x
}

fun shift : (i : Size) -> (Nat -> Maybe (SNat (succ i))) -> Nat -> Maybe (SNat i)
{

i f n = shift_case (f (succ infty n))

}

fun inc : Nat -> Maybe Nat
{

n = just (succ infty n)

}

data Unit : Set
{
	unit : Unit
}

mutual 
{

fun loop_case : (i : Size ) -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> Unit
{
i f nothing  = unit;
i f (just (zero a)) = unit;
i f (just (succ a y)) = loop i y (\(x:Nat) -> shift i f x)

}

fun loop : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
{

(s i) (zero a) f = loop_case i f (f (zero a));
(s i) (succ a n) f = loop i n  (\(x:Nat) -> shift i f x)

}

}

const diverge : Unit = loop infty (zero infty) inc
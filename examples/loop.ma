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

norec shift_case : (i : Size) -> Maybe (SNat ($ i)) -> Maybe (SNat i)
{

shift_case i (nothing .(SNat ($ i))) = nothing (SNat i);
shift_case .i (just .(SNat ($ i)) (zero i)) = nothing (SNat i);
shift_case .i (just .(SNat ($ i)) (succ i x)) = just (SNat i) x

}

norec shift : (i : Size) -> (Nat -> Maybe (SNat ($ i))) -> Nat -> Maybe (SNat i)
{

shift i f n = shift_case i (f (succ # n))

}

norec inc : Nat -> Maybe Nat
{

inc n = just Nat (succ # n)
}

data Unit : Set
{
	unit : Unit
}


mutual 
{

fun loop_case : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> Unit
{

loop_case i       x f (nothing .(SNat i)) = unit;
loop_case .($ i)  x f (just .(SNat ($ i))  (zero i)) = unit;
loop_case .($ i)  x f  (just .(SNat ($ i)) (succ i y)) = loop i y (shift i f) 

}

fun loop : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
{

loop .($ i) (zero i) f = loop_case ($ i) (zero i) f (f (zero i)); --weak #
loop .($ i) (succ i n) f = loop i n (shift i f)
}

}

const diverge : Unit = loop # (zero #) inc
data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat (s i);
	succ : (i : Size) -> SNat i -> SNat (s i)
}

data Maybe (A : Set ) : Set
{
  nothing : Maybe A;
  just : A -> Maybe A
}

const Nat : Set = SNat infty

fun shift_case : (i : Size) -> Maybe (SNat (s i)) -> Maybe (SNat i)
{

shift_case i (nothing .(SNat (s i))) = nothing (SNat i);
shift_case .i (just .(SNat (s i)) (zero i)) = nothing (SNat i);
shift_case .i (just .(SNat (s i)) (succ i x)) = just (SNat i) x

}

fun shift : (i : Size) -> (Nat -> Maybe (SNat (s i))) -> Nat -> Maybe (SNat i)
{

shift i f n = shift_case i (f (succ infty n))

}

fun inc : Nat -> Maybe Nat
{

inc n = just Nat (succ infty n)
}

data Unit : Set
{
	unit : Unit
}


mutual 
{

fun loop_case : (i : Size ) -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> Unit
{

loop_case i       f (nothing .(SNat i)) = unit;
loop_case .(s i)   f (just .(SNat (s i))  (zero i)) = unit;
loop_case .(s i)   f  (just .(SNat (s i)) (succ i y)) = loop i y (shift i f) 

}

fun loop : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
{

loop .(s i) (zero i) f = loop_case (s i) f (f (zero i)); --weak infty
loop .(s i) (succ i n) f = loop i n (shift i f)
}

}

const diverge : Unit = loop infty (zero infty) inc
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

mutual 
{

fun loop : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> Unit
{
loop .($ i) (zero i) f = loop_case ($ i) f (f (zero i)); 
loop .($ i) (succ i n) f = loop i n (shift i f)
}

fun loop_case : (i : Size ) -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> Unit
{
loop_case i       f (nothing .(SNat i)) = unit;
loop_case .($ i)  f (just .(SNat ($ i))  (zero i)) = unit;
loop_case .($ i)  f (just .(SNat ($ i)) (succ i y)) = loop i y (shift i f) 
}
}

const diverge : Unit = loop # (zero #) inc


const inc2 : (i : Size ) -> SNat i -> SNat ($ i) = \i -> \n -> succ i n  


fun blob : (i : Size ) -> SNat i -> (SNat i -> SNat i) -> SNat i 
{
blob i x f = f x
}
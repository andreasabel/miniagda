sized data SNat : Size -> Set
{
	zero : (i : Size ) -> SNat ($ i);
	succ : (i : Size) -> SNat i -> SNat ($ i)
}

data Maybe ( + A : Set ) : Set
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

data loopType : Set 
{
lt : (i : Size ) -> SNat i -> (Nat -> Maybe (SNat i)) -> loopType
}

data loopCaseType : Set
{
lct : (i : Size ) -> (Nat -> Maybe (SNat i)) -> Maybe (SNat i) -> loopCaseType
}


-- hide bad types ....
mutual 
{

fun loop : loopType -> Unit
{
loop (lt .($ i) (zero i) f) = loop_case (lct ($ i) f (f (zero i))); 
loop (lt .($ i) (succ i n) f) = loop (lt i n (shift i f))
}

fun loop_case : loopCaseType -> Unit 
{
loop_case (lct i f (nothing .(SNat i))) = unit;
loop_case (lct .($ i)  f (just .(SNat ($ i))  (zero i))) = unit;
loop_case (lct .($ i)  f (just .(SNat ($ i)) (succ i y))) = loop (lt i y (shift i f)) 
}
}

eval const diverge : Unit = loop (lt # (zero #) inc)
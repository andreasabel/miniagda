data Bool : Set
{
	tt : Bool;
	ff : Bool
}

fun apply : (Bool -> Bool) -> Bool -> Bool
{
apply f b = f b 
}

fun neg : Bool -> Bool
{
neg	tt = ff;
neg	ff = tt	
}

let f : Bool = apply neg tt

let t : Bool = apply (\ x  -> tt) ff


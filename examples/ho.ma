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

const f : Bool = apply neg tt

const t : Bool = apply (\(x:Bool) -> tt) ff


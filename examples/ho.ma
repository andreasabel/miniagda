data Bool : Set
{
	tt : Bool;
	ff : Bool
}

fun apply : (Bool -> Bool) -> Bool -> Bool
{
   f b = f b 
}

fun neg : Bool -> Bool
{
	tt = ff;
	ff = tt	
}

const f : Bool = apply neg tt

const t : Bool = apply (\(x:Bool) -> tt) ff


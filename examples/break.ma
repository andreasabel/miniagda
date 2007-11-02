data Bool : Set
{
tt: Bool;
ff : Bool
}

const bad : Set = (i : Size ) -> Bool

      
fun F : Bool -> Set
{
F tt = bad;
F ff = bad;
}

fun loop : (b : Bool ) -> F b 
{
loop tt ($ i) = loop tt i;
loop ff ($ i) = loop ff i;
}

eval const diverge : Bool = loop tt #
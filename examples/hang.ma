data Empty : Set
{
}

mutual 
{

-- should this scope check ? 
fun f  : Empty -> F Empty
{
f x = x
}

fun F : Set -> Set
{
F x = F x
}



}
data Empty : Set
{
}

mutual 
{

fun F : Set -> Set
{
F x = F x
}

-- should this scope check ? 
fun f  : Empty -> F Empty
{
f x = x
}

}
data Empty : Set
{
}

mutual 
{

fun f  : Empty -> F Empty
{
f x = x
}

fun F : Set -> Set
{
F x = F x
}



}
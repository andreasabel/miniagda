data Empty : Set
{
}

mutual 
{


fun F : Set -> Set
{
F x = F x
}

fun f  : Empty -> F Empty
{
f x = x
}

}
data Empty : Set
{
}

mutual
{

fun F : Empty -> Empty
{
F x = F x
}

-- should this scope check ?
fun f  : Empty -> Empty
{
f x = f (F x)
}

}

data List : Size -> Set
{
nil : (i : Size ) -> List i;
cons : (i : Size ) -> List i -> List ($ i);
}

data Empty : Set
{
}

fun foo : (i : Size ) -> List i -> Empty
{
foo .($ i) (nil ($ i)) = foo i (nil i);
foo .($ i) (cons i l) = foo i l
}

eval const loop : Empty = foo # (nil #)

data Bla : Set
{
bla : Bla
}

fun b : Bla -> Bla
{
b x = b x
}

--eval const loop2 : Bla = b bla
data List : Size -> Set
{
nil : (i : Size ) -> List i;
cons : (i : Size ) -> List i -> List ($ i);
}

data Empty : Set
{
}

fun foo : (i : Size ) -> List i -> List i
{
foo .i (nil i) = foo i (nil i);
foo .($ i) (cons .i (nil i)) = foo i (nil i);
foo .($ i) (cons i xl) = foo i xl
}

eval const loop : List # = foo # (nil #)


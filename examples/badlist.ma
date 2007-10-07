data List : Size -> Set
{
nil : (i : Size ) -> List i;
cons : (i : Size ) -> List i -> List ($ i);
}

fun foo : (i : Size ) -> List i -> List i
{
foo .($ i) (nil ($ i)) = foo i (nil i); -- not possible w/o subtyping
foo .i (nil i) = nil i;
foo .($ i)y (cons i l) = foo i l
}

--const loop : List # = foo # (nil #)
data List : Size -> Set
{
nil : (i : Size ) -> List i;
cons : (i : Size ) -> List i -> List ($ i);
}

data Empty : Set
{
}


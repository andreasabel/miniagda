data SList (A : Set ) : Size -> Set
{
nil : ( i : Size ) -> SList A ($ i);
cons : (i : Size ) -> A -> SList A i -> SList A ($ i)
}

fun mapL : (i : Size ) -> ( A : Set ) -> ( B : Set ) -> ( A -> B ) -> SList A i -> SList B i
{
mapL .($ i) .A B f (nil A i) = (nil B i); 
mapL .($ i) .A B f (cons A i x xl) = cons B i (f x) (mapL i A B f xl)
}

data Tree (A : Set) : Size -> Set 
{
  leaf : (i : Size ) -> A -> Tree A ($ i);
  node : (i : Size ) -> SList (Tree A i) # -> Tree A ($ i)
}


fun mapT : ( A : Set ) -> (B : Set ) -> (i : Size ) -> (A -> B) -> Tree A i -> Tree B i
{
mapT .A B .($ i) f (leaf A i a) = leaf B i (f a);
mapT .A B .($ i) f (node A i l) = node B i (mapL # (Tree A i) (Tree B i) (mapT A B i f) l)
}


-- append list

fun append : (i : Size ) -> ( A : Set ) -> SList A i -> SList A # -> SList A #
{
append .($ i) .A (nil A i) yl = yl ;
append .($ i) .A (cons A i x xl) yl = cons A # x (append i A xl yl) 
}


-- concatenate lists
fun conc : (i : Size ) -> ( A : Set ) -> SList (SList A #) i -> SList A #
{
conc .($ i) A (nil .(SList A #) i) = nil A # ;
conc .($ i) A (cons .(SList A #) i l l2) = append # A l (conc i A l2)
}

-- tree flattening

fun flat : ( i : Size ) -> ( A : Set ) -> Tree A i -> SList A # 
{
flat .($ i) .A (leaf A i a) = cons A ($ i) a (nil A i);
flat .($ i) .A (node A i l) = conc # A (mapL # (Tree A i) (SList A #) (flat i A) l)
}
data SList (A : Set ) : Size -> Set
{
nil : ( i : Size ) -> SList A (s i);
cons : (i : Size ) -> A -> SList A i -> SList A (s i)
}

fun mapL : (i : Size ) -> ( A : Set ) -> ( B : Set ) -> ( A -> B ) -> SList A i -> SList B i
{
mapL .(s i) .A B f (nil A i) = (nil B i); 
mapL .(s i) .A B f (cons A i x xl) = cons B i (f x) (mapL i A B f xl)
}

data Tree (A : Set) : Size -> Set 
{
  leaf : (i : Size ) -> A -> Tree A (s i);
  node : (i : Size ) -> SList (Tree A i) infty -> Tree A (s i)
}


fun mapT : ( A : Set ) -> (B : Set ) -> (i : Size ) -> (A -> B) -> Tree A i -> Tree B i
{
mapT .A B .(s i) f (leaf A i a) = leaf B i (f a);
mapT .A B .(s i) f (node A i l) = node B i (mapL infty (Tree A i) (Tree B i) (mapT A B i f) l)
}


-- append list

fun append : (i : Size ) -> ( A : Set ) -> SList A i -> SList A infty -> SList A infty
{
append .(s i) .A (nil A i) yl = yl ;
append .(s i) .A (cons A i x xl) yl = cons A infty x (append i A xl yl) 
}


-- concatenate lists
fun conc : (i : Size ) -> ( A : Set ) -> SList (SList A infty ) i -> SList A infty
{
conc .(s i) A (nil .(SList A infty) i) = nil A infty ;
conc .(s i) A (cons .(SList A infty) i l l2) = append infty A l (conc i A l2)
}

-- tree flattening

fun flat : ( i : Size ) -> ( A : Set ) -> Tree A i -> SList A infty 
{
flat .(s i) .A (leaf A i a) = cons A (s i) a (nil A i);
flat .(s i) .A (node A i l) = conc infty A (mapL infty (Tree A i) (SList A infty) (flat i A) l)
}
data List ( + A : Set ) : Set
{
nil : List A;
cons : A -> List A -> List A
}

fun mapL : ( A : Set ) -> ( B : Set ) -> ( A -> B ) -> List A -> List B
{
mapL .A B f (nil A) = (nil B); 
mapL .A B f (cons A x xl) = cons B (f x) (mapL A B f xl)
}

data Tree (+ A : Set) : Set 
{
  leaf : A -> Tree A;
  node : List (Tree A) -> Tree A
}

fun mapT : ( A : Set ) -> (B : Set ) -> (A -> B) -> Tree A -> Tree B
{
mapT .A B f (leaf A a) = leaf B (f a);
mapT .A B f (node A l) = node B (mapL (Tree A) (Tree B) (mapT A B f) l)
}

-- append list

fun append : ( A : Set ) -> List A -> List A -> List A
{
append .A (nil  A) yl = yl ;
append .A (cons A x xl) yl = cons A x (append A xl yl) 
}


-- concatenate lists
fun conc : ( A : Set ) -> List (List A) -> List A
{
conc A (nil .(List A)) = nil A ;
conc A (cons .(List A) l l2) = append A l (conc A l2)
}

-- tree flattening

fun flat : ( A : Set ) -> Tree A -> List A 
{
flat .A (leaf A a) = cons A a (nil A);
flat .A (node A l) = conc A (mapL (Tree A) (List A) (flat A) l)
}
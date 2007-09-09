data List (A : Set ) : Set
{
nil : List A;
cons : A -> List A -> List A
}

fun mapL : ( A : Set ) -> ( B : Set ) -> ( A -> B ) -> List A -> List B
{
mapL .A B f (nil A) = (nil B); 
mapL .A B f (cons A x xl) = cons B (f x) (mapL A B f xl)
}

data Tree (A : Set) : Size -> Set 
{
  leaf : (i : Size ) -> A -> Tree A (s i);
  node : (i : Size ) -> List (Tree A i) -> Tree A (s i)
}

fun mapT : ( A : Set ) -> (B : Set ) -> (i : Size ) -> (A -> B) -> Tree A i -> Tree B i
{
mapT .A B .(s i) f (leaf A i a) = leaf B i (f a);
mapT .A B .(s i) f (node A i l) = node B i (mapL (Tree A i) (Tree B i) (mapT A B i f) l)
}

data Enum : Set
{
aa : Enum;
bb : Enum;
cc : Enum
}

fun perm : Enum -> Enum
{
perm aa = bb;
perm bb = cc;
perm cc = aa
}

const TE : Set = Tree Enum infty 
const aal : TE = leaf Enum infty aa
const bbl : TE = leaf Enum infty bb
const ccl : TE = leaf Enum infty cc

const t1 : TE = node Enum infty (cons TE aal (cons TE bbl (cons TE ccl (nil TE))))
const t2 : TE = node Enum infty (cons TE aal (cons TE t1 (nil TE)))
const t3 : TE = mapT Enum Enum infty perm t2
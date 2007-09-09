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

data Tree (A : Set) : Set 
{
  leaf : A -> Tree A;
  node : List (Tree A) -> Tree A
}

fun mapT : ( A : Set ) -> (B : Set ) -> (A -> B) -> Tree A -> Tree B
{
mapT .A B f (leaf A a) = leaf B (f a);
mapT .A B f (node A l) = node B (mapL (Tree A) (Tree B) (mapT A B f) l)
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

const TE : Set = Tree Enum
const aal : TE = leaf Enum aa
const bbl : TE = leaf Enum bb
const ccl : TE = leaf Enum cc

const t1 : TE = node Enum (cons TE aal (cons TE bbl (cons TE ccl (nil TE))))
const t2 : TE = node Enum (cons TE aal (cons TE t1 (nil TE)))
const t3 : TE = mapT Enum Enum perm t2
data List ( A : Set ) : Set
{
nil : List A;
cons : A -> List A -> List A
}

fun append : ( A : Set ) -> List A -> List A -> List A
{
append A (nil .A) l = l;
append A (cons .A x xl) l = cons A x (append A xl l)  
}

data Rose ( A : Set ) : Set
{
rose : A -> List (Rose A) -> Rose A
}

fun bf0 : (A : Set ) -> List (Rose A) -> List A
{
bf0 A (nil .(Rose A)) = nil A;
bf0 A (cons .(Rose A) (rose .A a rs) rs' ) = 
  cons A a (bf0 A (append (Rose A) rs' rs))
}
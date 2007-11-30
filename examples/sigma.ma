data Sigma ( A: Set ) ( B : A -> Set ) : Set
{
sigma : (a : A ) -> (b : B a ) -> Sigma A B 
}

fun pr1 : (A : Set ) -> ( B : A -> Set ) -> Sigma A B -> A
{
pr1 .A .B (sigma A B a b) = a
}

fun pr2 : ( A : Set ) -> ( B : A -> Set ) -> (s : Sigma A B) -> B (pr1 A B s) 
{
pr2 .A .B (sigma A B a b) = b   
}
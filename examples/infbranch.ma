data Nat : Set 
{
zero : Nat;
succ : Nat -> Nat
}

data InfTree (A : Set ) : Set
{
leaf : A -> InfTree A ;
node : (Nat -> InfTree A) -> InfTree A 
}

fun map : ( A : Set ) -> ( B : Set ) -> ( A -> B ) -> InfTree A -> InfTree B 
{
map .A B f (leaf A a) = leaf B (f a);
map .A B f (node A g) = node B (\x -> (map A B f (g x)))
}




data Eq (A : Set ) (a  : A) : A -> Set 
{
refl : Eq A a a 
}

data Nat : Set
{
succ : Nat -> Nat;
zero : Nat
}


mutual 
{


fun f : ( x : Nat )  -> Eq Nat x zero  
{
f x = refl Nat (g # x)
}

-- not type correct, but the termination checker does not see that
-- solution: don't enable "execution" of g until completion of type checking.
fun g : (i : Size ) -> Nat -> Nat
{
g .($ x) x = g x x 
}

}




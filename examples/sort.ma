data Nat : Set
{
	zero : Nat;
	succ : Nat -> Nat
}

data List ( A : Set ) : Nat -> Set
{
	nil : List A zero ;
	cons : (n : Nat) -> A -> List A n -> List A (succ n)
}

data Bool : Set
{
	tt : Bool;
	ff : Bool
}

fun ite : ( A : Set) -> Bool -> A -> A -> A
{
ite A true x y = x;
ite A false x y = y
} 

fun insert : ( A : Set ) -> ( leq : ( A -> A -> Bool ) ) -> A -> ( n : Nat ) -> List A n  -> List A (succ n)
{
insert A leq x .zero    (nil .A)          = cons A zero x (nil A);
insert A leq x (succ n) (cons .A .n y ys) = ite (List A (succ(succ n))) (leq x y) 
					      (cons A (succ n) y (cons A n y ys)) 
				 	      (cons A (succ n) y (cons A n y ys)) 
}

fun sort : ( A : Set ) -> ( leq : ( A -> A -> Bool)) -> ( n : Nat) -> List A n -> List A n 
{
sort A leq .zero (nil .A) = nil A; 
sort A leq (succ n) (cons .A .n y ys) = insert A leq y n (sort A leq n ys)  
}
data List ( A : Set ) : Set 
{
  nil : List A ;
  cons : A -> List A -> List A
}

data Prod (A : Set) : Set 
{
  prod : A -> A -> Prod A 
}

fun pr1 : ( A : Set ) -> Prod A -> A
{
pr1 .A (prod A a b) = a
}

fun pr2 : ( A : Set ) -> Prod A -> A
{
pr2 .A (prod A a b) = b
}

data Bool : Set 
{
  tt : Bool;
  ff : Bool
}

fun ite : (A : Set ) -> Bool -> A -> A -> A
{
ite A tt x y = x;
ite A ff x y = y
}


fun pivot : (A : Set ) -> ( leq : A -> A -> Bool ) -> A -> List A -> Prod (List A)
{
pivot .A leq a (nil A) = prod (List A) (nil A) (nil A);
pivot .A leq a (cons A x xs) = ite (Prod (List A)) (leq a x) 
   
     (prod (List A)
	(pr1 (List A) (pivot A leq a xs))
	(cons A x (pr2 (List A) (pivot A leq a xs))) 
     )
     (prod (List A)
	(cons A x (pr1 (List A) (pivot A leq a xs))) 
	(pr2 (List A) (pivot A leq a xs))
     )
}


fun qsapp : ( A : Set ) -> ( leq : A -> A -> Bool ) -> List A -> List A -> List A
{
qsapp .A leq (nil A) ys = ys;
qsapp .A leq (cons A x xs) ys = qsapp A leq 
	(pr1 (List A) (pivot A leq x xs))
        (cons A x (qsapp A leq (pr2 (List A) (pivot A leq x xs)) ys))
}

fun quicksort : (A : Set ) -> (leq : A -> A -> Bool) -> List A -> List A
{
quicksort A leq l = qsapp A leq l (nil A) 
}


data Nat : Set 
{
  succ : Nat -> Nat;
  zero : Nat
}

fun leqN : Nat -> Nat -> Bool
{
leqN zero _ = tt;
leqN (succ n) zero = ff;
leqN (succ n) (succ m) = leqN n m
}

const one : Nat = succ zero
const two : Nat = succ one
const three : Nat = succ two
const four : Nat = succ three

const l1 : List Nat = cons Nat two (cons Nat three (cons Nat four (cons Nat one (nil Nat)))) 
const sl1 : List Nat = quicksort Nat leqN l1
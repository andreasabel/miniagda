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
pivot .A leq a (cons A x xs) =  let rec : Prod (List A) = (pivot A leq a xs) in

      ite (Prod (List A)) (leq a x) 
     
        (prod (List A)
	  (pr1 (List A) rec)
	   (cons A x (pr2 (List A) rec)) 
     	)	   
        
        (prod (List A)
	  (cons A x (pr1 (List A) rec)) 
	  (pr2 (List A) rec)
        )
}


fun qsapp : ( A : Set ) -> ( leq : A -> A -> Bool ) -> List A -> List A -> List A
{
qsapp .A leq (nil A) ys = ys;
qsapp .A leq (cons A x xs) ys = let pv : Prod (List A) = pivot A leq x xs in
        qsapp A leq  
	  (pr1 (List A) pv)
          (cons A x (qsapp A leq (pr2 (List A) pv) ys))
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
leqN zero m = tt;
leqN (succ n) zero = ff;
leqN (succ n) (succ m) = leqN n m
}

let one : Nat = succ zero
let two : Nat = succ one
let three : Nat = succ two
let four : Nat = succ three

let l1 : List Nat = cons Nat two (cons Nat three (cons Nat four (cons Nat one (nil Nat)))) 
let sl1 : List Nat = quicksort Nat leqN l1


-------


fun filter : ( A : Set ) -> (p : A -> Bool ) -> List A -> List A  
{
filter A p (nil .A) = nil A ;
filter A p (cons .A x xs) = 
       let rec : List A = filter A p xs in
         ite (List A) (p x) (cons A x rec) rec
}
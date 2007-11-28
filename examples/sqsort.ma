sized data List ( + A : Set ) : Size -> Set 
{
  nil : (i : Size ) -> List A ($ i);
  cons : (i : Size ) -> A -> List A i -> List A ($ i)
}

data Prod ( + A : Set) : Set 
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

fun pivot : (i : Size ) -> (A : Set ) -> ( leq : A -> A -> Bool ) 
	-> A -> List A i -> Prod (List A i)
{
pivot .($ i)     .A leq a (nil A i) = prod (List A ($ i)) (nil A i) (nil A i);

pivot .($ i)     .A leq a (cons A i x xs) = 
  let rec : Prod (List A i) = (pivot i A leq a xs) in
  let l1 : List A i = pr1 (List A i) rec in
  let l2 : List A i = pr2 (List A i) rec in  
     ite (Prod (List A ($ i))) (leq a x)  
      (prod (List A ($ i)) l1  (cons A i x l2))
      (prod (List A ($ i)) (cons A i x l1) l2)
}

fun qsapp : (i : Size ) -> ( A : Set ) -> ( leq : A -> A -> Bool ) 
	-> List A i -> List A # -> List A #
{
qsapp .($ i) .A leq (nil A i)       ys = ys;
qsapp .($ i) .A leq (cons A i x xs) ys = 
  let pv : Prod (List A i) = pivot i A leq x xs  in
  let l1 : List A i = pr1 (List A i) pv  in
  let l2 : List A i = pr2 (List A i) pv  in      
           qsapp i A leq l1 (cons A # x (qsapp i A leq l2 ys))
}

fun quicksort : (i : Size ) -> (A : Set ) -> (leq : A -> A -> Bool) 
	-> List A i -> List A #
{
  quicksort i A leq l = qsapp i A leq l (nil A #) 
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

eval let l1 : List Nat # = cons Nat # two (cons Nat # three (cons Nat # one (nil Nat #)))
 
eval let sl1 : List Nat # = quicksort # Nat leqN l1


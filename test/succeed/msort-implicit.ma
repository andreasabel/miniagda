-- erased arguments
-- in the spirit of the implicit CC
-- we only specify the erasure in the types


-- booleans

data Bool : Set 
{
  tt : Bool;
  ff : Bool
}

fun ifthenelse : [A : Set] -> Bool -> A -> A -> A
{
  ifthenelse A tt x y = x;
  ifthenelse A ff x y = y
}

-- homogeneous pairs

data Pair (+ A : Set) : Set 
{
  pair : A -> A -> Pair A 
}
-- this yields
--
--   pair : [A : Set] -> A -> A -> Pair A
--
-- parameter arguments to constructors are always implicit

fun pr1 : [A : Set] -> Pair A -> A
{
  pr1 .A (pair A a b) = a
}

fun pr2 : [A : Set] -> Pair A -> A
{
  pr2 .A (pair A a b) = b
}

-- sized Lists

sized data SList (+ A : Set) : Size -> Set 
{
  nil  : [i : Size] -> SList A ($ i) ;
  cons : [i : Size] -> A -> SList A i -> SList A ($ i)
}

-- merge sort

fun split : [A : Set] -> 
            [i : Size] -> SList A i -> Pair (SList A i)
{
split .A .($ i)     (nil A i)                    
  = pair (SList A _) (nil A _) (nil A _);

split .A .($ ($ i)) (cons .A .($ i) a (nil A i))
  = pair (SList A _) (cons A _ a (nil A _)) (nil A _);

split .A .($ ($ i)) (cons .A .($ i) a (cons A i b as))  
  =  let rec : Pair (SList A i) = split A _ as
  in let l1 : SList A _ = pr1 (SList A _) rec
  in let l2 : SList A _ = pr2 (SList A _) rec
  in pair (SList A _) (cons A _ a l1) (cons A _ b l2)
}

fun merge : [A : Set] -> (leq : A -> A -> Bool) 
            -> SList A # -> SList A # -> SList A #
{
merge .A leq (nil A .#) ys = ys;
merge .A leq (cons A .# x xs) (nil .A .#) = cons A _ x xs;
merge .A leq (cons A .# x xs) (cons .A .# y ys) = ifthenelse (SList A _)
	(leq x y) (cons A _ x (cons A _ y (merge A leq xs ys)))
		  (cons A _ y (cons A _ x (merge A leq xs ys)))
}

fun msort : [A : Set] -> (leq : A -> A -> Bool) ->
            [i : Size] -> SList A i -> SList A #
{
  msort .A leq .($ j) (nil A j) = nil A _ ;
  msort .A leq .($ ($ i)) (cons .A .($ i) a (nil A i)) = 
     cons A _ a (nil A _) ;
  msort .A leq .($ ($ i)) (cons .A .($ i) a (cons A i b l)) =
        let sl : Pair (SList A _) = split A _ l      
     in let l1 : SList A # = msort A leq _ (cons A _ a (pr1 (SList A _) sl))
     in let l2 : SList A # = msort A leq _ (cons A _ b (pr2 (SList A _) sl))
     in merge A leq l1 l2
}


fun msort' : [A : Set] -> (leq : A -> A -> Bool) ->
             ([i : Size] -> SList A i -> Pair (SList A i)) ->
             [i : Size] -> SList A i -> SList A #
{
  msort' .A leq splt .($ j) (nil A j) = nil A _ ;
  msort' .A leq splt .($ ($ i)) (cons .A .($ i) a (nil A i)) = 
     cons A _ a (nil A _) ;
  msort' .A leq splt .($ ($ i)) (cons .A .($ i) a (cons A i b l)) =
        let sl : Pair (SList A _) = splt _ l      
     in let l1 : SList A # = msort' A leq splt _ (cons A _ a (pr1 (SList A _) sl))
     in let l2 : SList A # = msort' A leq splt _ (cons A _ b (pr2 (SList A _) sl))
     in merge A leq l1 l2
}


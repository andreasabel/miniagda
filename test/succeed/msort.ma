-- 2012-01-22 parameters gone from constructors

-- booleans

data Bool : Set 
{
  tt : Bool;
  ff : Bool
}

fun ifthenelse : (A : Set) -> Bool -> A -> A -> A
{
  ifthenelse A tt x y = x;
  ifthenelse A ff x y = y
}

-- homogeneous pairs

data Pair (+ A : Set) : Set 
{
  pair : A -> A -> Pair A 
}

fun pr1 : (A : Set) -> Pair A -> A
{
  pr1 A (pair a b) = a
}

fun pr2 : (A : Set) -> Pair A -> A
{
  pr2 A (pair a b) = b
}

-- sized Lists

sized data SList (+ A : Set) : Size -> Set 
{
  nil  : (i : Size) -> SList A ($ i) ;
  cons : (i : Size) -> A -> SList A i -> SList A ($ i)
}

-- merge sort

fun split : (A : Set) -> 
            (i : Size) -> SList A i -> Pair (SList A i)
{
split A .($ i)     (nil i)                    
  = pair (nil _) (nil _);

split A .($ ($ i)) (cons .($ i) a (nil i))
  = pair (cons _ a (nil _)) (nil _);

split A .($ ($ i)) (cons .($ i) a (cons i b as))  
  =  let rec : Pair (SList A i) = split A _ as
  in let l1 : SList A _ = pr1 (SList A _) rec
  in let l2 : SList A _ = pr2 (SList A _) rec
  in pair (cons _ a l1) (cons _ b l2)
}

fun merge : (A : Set) -> (leq : A -> A -> Bool) 
            -> SList A # -> SList A # -> SList A #
{
merge A leq (nil .#) ys = ys;
merge A leq (cons .# x xs) (nil .#) = cons _ x xs;
merge A leq (cons .# x xs) (cons .# y ys) = ifthenelse (SList A _)
	(leq x y) (cons _ x (cons _ y (merge A leq xs ys)))
		  (cons _ y (cons _ x (merge A leq xs ys)))
}

fun msort : (A : Set) -> (leq : A -> A -> Bool) ->
            (i : Size) -> SList A i -> SList A #
{
  msort A leq .($ j) (nil j) = nil _ ;
  msort A leq .($ ($ i)) (cons .($ i) a (nil i)) = 
     cons _ a (nil _) ;
  msort A leq .($ ($ i)) (cons .($ i) a (cons i b l)) =
        let sl : Pair (SList A _) = split A _ l      
     in let l1 : SList A # = msort A leq _ (cons _ a (pr1 (SList A _) sl))
     in let l2 : SList A # = msort A leq _ (cons _ b (pr2 (SList A _) sl))
     in merge A leq l1 l2
}


fun msort' : (A : Set) -> (leq : A -> A -> Bool) ->
             ((i : Size) -> SList A i -> Pair (SList A i)) ->
             (i : Size) -> SList A i -> SList A #
{
  msort' A leq splt .($ j) (nil j) = nil _ ;
  msort' A leq splt .($ ($ i)) (cons .($ i) a (nil i)) = 
     cons _ a (nil _) ;
  msort' A leq splt .($ ($ i)) (cons .($ i) a (cons i b l)) =
        let sl : Pair (SList A _) = splt _ l      
     in let l1 : SList A # = msort' A leq splt _ (cons _ a (pr1 (SList A _) sl))
     in let l2 : SList A # = msort' A leq splt _ (cons _ b (pr2 (SList A _) sl))
     in merge A leq l1 l2
}


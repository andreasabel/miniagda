data Prod (+A : Set) (+B : Set) : Set
{
  pair : A -> B -> Prod A B
}

fun split : (A : Set) -> (B : Set) -> Prod A B ->  
            (C : Set) -> (A -> B -> C) -> C  
{
  split A B (pair .A .B a b) C f = f a b
}

sized data List (+ A : Set) : Size -> Set 
{
  nil  : (i : Size) -> List A ($ i) ;
  cons : (i : Size) -> A -> List A i -> List A ($ i)
}

fun append : (A : Set) -> List A # -> List A # -> List A #
{
  append A (nil .A .#) l = l;
  append A (cons .A .# a as) l = cons A # a (append A as l)
}

sized data Rose (+A : Set) : Size -> Set
{
  rose : (i : Size) -> A -> List (Rose A i) # -> Rose A ($ i)
}



fun step : (j : Size) -> (A : Set) -> (i : Size) ->
           List (Rose A ($ i)) j -> 
           Prod (List A j) (List (Rose A i) #) 

{
  step .($ j) A i (nil .(Rose A ($ i)) j) = 
    pair (List A _) (List (Rose A _) _) 
      (nil A _) 
      (nil (Rose A i) _);

  step .($ j) A .i (cons .(Rose A ($ i)) j (rose .A i a rs') rs) =
    split (List A j) (List (Rose A i) #) 
       (step j A i rs) 
          (Prod (List A ($ j)) (List (Rose A i) #))
       (\ as -> \ rs'' -> pair (List A _) (List (Rose A _) #) 
           (cons A _ a as) 
           (append (Rose A i) rs' rs'')) 

}

fun bf' : (A : Set) -> (i : Size) -> List A # -> List (Rose A i) # -> List A # 
{
  bf' A i as (nil .(Rose A i) .#) = as;
  bf' A .($ i) as (cons .(Rose A ($ i)) .# (rose .A i a r) rs) = append A as 
    (split
        (List A #) (List (Rose A i) #) 
      (step # A i (cons (Rose A ($ i)) _ (rose A _ a r) rs))
        (List A #) 
      (bf' A i) 
    )
}

{-
mutual {

  fun bf' : (A : Set) -> (i : Size) -> List A # -> List (Rose A i) # -> List A # 
  {
    bf' A ($ i) as (nil .(Rose A ($ i)) .#) = as;
    bf' A ($ i) as (cons .(Rose A ($ i)) .# r rs) =
      append A as (bf A i r rs)
  }
  
  fun bf : (A : Set) -> (i : Size) -> Rose A i -> List (Rose A i) # -> List A #
  {
  
    bf A i r rs = 
      (split
          (List A #) (List (Rose A i) #) 
        (step # A i (cons (Rose A ($ i)) _ r rs))
          (List A #) 
        (bf' A i) 
      )
  }
  
}

-}
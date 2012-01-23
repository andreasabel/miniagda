-- 2010-09-06 Anton Setzer on coalgebras and destructor patterns

{- 

record Stream (A : Set) : Size -> Set
{ head : [i : Size] -> Stream A $i -> A
; tail : [i : Size] -> Stream A $i -> Stream A i
}

constructor:

  record { head = ..., tail = ... }

  new Stream hd tl

record Stream (A : Set) : Size -> Set where
  constructor mkStream
  destructors
    head : [i : Size] -> Stream A $i -> A
    tail : [i : Size] -> Stream A $i -> Stream A i

can the destructors have different index patterns?  Why not!

   

-}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data Prod ++(A, B : Set) : Set 
{ pair : (fst : A) -> (snd : B) -> Prod A B
}
fields fst, snd

{-
codata Stack (A : Set) : Nat -> Set
{ push : [n : Nat] -> 
   (top : A) ->
   (pop : Stack A n) ->
   Stack A (succ n)
}
-}

sized codata Stack (A : Set) : Size -> Nat -> Set
{ push : [i : Size] -> [n : Nat] -> 
   (top : A) ->
   (pop : Stack A i n) ->
   Stack A $i (succ n)
} 
fields top, pop

{-
fun push : [A : Set] -> [i : Size] -> [n : Nat] -> 
           A -> Stack A i n -> Stack A $i (succ n)
-}

-- cofun empty : Stack A # zero { () } -- ??

{- Conor: separate equality from inductive types -}

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a }

sized codata Stack' (A : Set)(n : Nat) : Size -> Set
{ mkStack : [i : Size] ->
   ([m : Nat] -> [Id Nat n (succ m)] -> Prod A (Stack' A m i)) ->
   Stack' A n $i 
}

fun absurd : [m : Nat] -> [Id Nat zero (succ m)] -> [A : Set] -> A
{ absurd m ()
} 

cofun empty : [A : Set] -> [i : Size] -> Stack' A zero i
{ empty A ($i) = mkStack {-A zero-} i (\ m p -> absurd m p (Prod A (Stack' A m i))) 
}

-- 2012-01-26 F# forward |> and backward pipe operators (<| is Haskell's $)

-- Backward pipe <|

-- backward pipe is a synonym for application, but associates to the right
-- and binds weaker than almost everything, exept ','
-- currently, it has same binding strength as -> and +

let three [A : Set] (f : A -> A) (x : A) : A
  = f <| f <| f x

let sbla (f : Size -> Size) (x, y : Size) -- : Size
  = f <| x + y

let threeId (f : [A : Set] -> A -> A) [A : Set] (x : A) -- : A
  = f A <| f A <| f A x

-- since <| and -> both associate to the right
-- first-come-first-serve

fail
let failure [F : Size -> Set] [i : Size] [B : Set] (x : F <| i -> B) : Size
  = 0
  -- parsed as F (i -> B)

let success [F : Size -> Set] [i : Size] [B : Set] (x : B -> F <| i) : Size
  = 0
  -- parsed as B -> F i

let one [A : Set] (f : A -> A) : A -> A 
  = \ x -> f <| x
  -- parsed as \ x -> f x

-- Forward pipe |>

let binApp [A,B,C : Set] (f : A -> B -> C) (x : A) (y : B) : C
  = y |> f x
  -- parsed as f x y

let redex [A : Set] : A -> A
  = \ x -> x |> \ y -> y
  -- parsed as \ x -> (\ y -> y) x

data List (A : Set) : Set 
{ nil                             : List A
; cons (head : A) (tail : List A) : List A
}

-- pipe back can be used in patterns
fun evens : [A : Set] -> List A -> List A
{ evens A nil = nil
; evens A <| cons x <| nil = nil
; evens A <| cons x <| cons y <| xs = cons x <| evens A xs
} 

-- ever tried parens?
{- fails
fun K : [A, B : Set] -> A -> B -> A
{ ((K A) B a) b = a
}
-}

record Prod ++(A, B : Set) : Set 
{ pair (fst : A) (snd : B) : Prod A B
} fields fst, snd

-- pointless but parses
fun fork : [A : Set] -> (a : A) -> Prod A A
{ fork A a <| .fst = a
; fork A a <| .snd = a
}

{- fails rightly, parsed as (a. fst)
fun fork' : [A : Set] -> (a : A) -> Prod A A
{ fork' A <| a .fst = a
; fork' A <| a .snd = a
}
-}



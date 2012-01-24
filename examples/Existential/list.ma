-- 2012-01-21

-- * empty type

data Empty : Set {}

fun abort : [i : Size] -> (A : Set i) -> .Empty -> A
{ abort i A ()
}

-- * unit type

record Unit : Set 
{ unit : Unit
}

-- * Booleans

data Bool : Set 
{ true  : Bool
; false : Bool
}

fun if : [i : Size] -> (A : Set i) -> Bool -> ++(a, b : A) -> A
{ if i A true  a b = a
; if i A false a b = b
}

fun If : Bool -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

-- * disjoint sum

let Either ++(A, B : Set) : Set
  = (b : Bool) & If b A B

pattern left  a = (true  , a)
pattern right b = (false , b)

{-
constructors [A, B : Set] 
{ left  (a : A) : Either A B = true  , a
; right (b : B) : Either A B = false , b 
}
-}

-- * Maybe

let Maybe : ++(A : Set) -> Set
  = Either Unit

pattern nothing = left unit
pattern just a  = right a


fun maybe : [A, B : Set] -> B -> (A -> B) -> Maybe A -> B
{ maybe A B b f nothing  = b
; maybe A B b f (just a) = f a
}

let mapMaybe : [A, B : Set] -> (A -> B) -> Maybe A -> Maybe B
  = \ A B f -> maybe A (Maybe B) nothing (\ a -> just (f a))
{-
fun mapMaybe : [A, B : Set] -> (A -> B) -> Maybe A -> Maybe B
{ mapMaybe A B f (true,  u) = (true, u)
; mapMaybe A B f (false, a) = (false, f a)
}
-}

-- * Lists

let ListF : ++(A, X : Set) -> Set
  = \ A X -> Maybe (A & X)

cofun List : ++(A : Set) -> ++(i : Size) -> Set
{ List A i = (j < i) & ListF A (List A j)
}

pattern nil       = nothing
pattern cons x xs = just (x , xs)

-- * fold right and instances

fun foldr : [A : Set] -> [B : +Size -> Set] -> 
  ([j : Size] -> A -> B j -> B $j) ->
  ([j : Size] -> B $j) -> 
  [i : Size] -> List A i -> B i
{ foldr A B f b i (j < i , nothing)   = b j
; foldr A B f b i (j < i , cons a as) = f j a (foldr A B f b j as)
}

let mapList : [A, B : Set] -> (A -> B) -> [i : Size] -> List A i -> List B i
  = \ A B f -> foldr A (List B) 
       (\ j a b -> j , cons (f a) b)   
       (\ j     -> j , nothing)
{-
fun mapList : [A, B : Set] -> (A -> B) -> [i : Size] -> List A i -> List B i
{ mapList A B f i (j < i , true ,  u)      = j , true , u
; mapList A B f i (j < i , false , a , as) = j , false , f a , mapList A B f j as
}
-}

let append : [A : Set] -> [i, j : Size] -> List A i -> List A $j -> List A (i+j)
  = \ A i j as bs -> 
      foldr A (\ i -> List A (i+j)) 
        (\ i b bs -> (i+j , cons b bs)) 
        (\ i -> bs) 
        i 
        as

-- * fold left: looses size information
--     otherwise: reverse would have size info

{-
fun foldl : [A : Set] -> [B : +Size -> Set] -> [i : Size] ->
  ([j : Size] -> B i -> A -> B j) -> 
  List A i ->  B i -> B i 
{ foldl A B i f (j < i , true  , u)      = \ acc -> acc
; foldl A B i f (j < i , false , a , as) = \ acc -> foldl A B j f (f j acc a) as
}
-}

let foldl' : [A : Set] -> [B : Set] -> (B -> A -> B) -> 
  [i : Size] -> List A i -> B -> B
  = \ A B f -> foldr A (\ j -> B -> B) 
      (\ j a r acc -> r (f acc a))
      (\ j acc -> acc)

let foldl : [A : Set] -> [B : Set] -> (B -> A -> B) -> B ->
  [i : Size] -> List A i -> B
  = \ A B f b i l -> foldl' A B f i l b      
      
{-
fun foldl' : [A : Set] -> [B : Set] -> [i : Size] ->
  (B -> A -> B) -> 
  List A i -> B -> B
{ foldl' A B i f (j < i , true  , u)      = \ acc -> acc
; foldl' A B i f (j < i , false , a , as) = \ acc -> foldl' A B j f as (f acc a)
}
-}

let LIST : ++(A : Set) -> Set
  = \ A -> (i : Size) & List A i

let NIL  : [A : Set] -> LIST A
  = \ A -> 1 , 0 , nil

fun CONS : [A : Set] -> A -> LIST A -> LIST A
{ CONS A a (j , as) = $j , j , cons a as
}

fun revApp : [A : Set] -> LIST A -> LIST A -> LIST A 
{ revApp A (i , as) bs = foldl A (LIST A) (\ as a -> CONS A a as) bs i as
} 
let reverse : [A : Set] -> LIST A -> LIST A 
  = \ A as -> revApp A as (NIL A)

{-
fun reverse : [A : Set] -> LIST A -> LIST A 
{ reverse A (i , as) = foldl A (LIST A) (\ as a -> CONS A a as) (NIL A) i as
} 
-}

-- 2012-01-28 Stream processors as a mixed coinductive-inductive type

-- * Booleans

data Bool { true ; false }

fun If : Bool -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

-- * disjoint sum

let Either ++(A, B : Set) = (b : Bool) & If b A B

pattern left  a = (true  , a)
pattern right b = (false , b)

-- * streams defined recursively

cofun Stream : ++(A : Set) -> -(i : Size) -> Set
{ Stream A i = [j < i] -> A & (Stream A j)
}

pattern cons x xs = x , xs

-- * Infinity streams

let head [A : Set] (s : Stream A #) : A
  = case (s 0) { (cons x xs) -> x }

fun tail : [A : Set] -> Stream A # -> Stream A #
{ tail A s (j < #) = case (s $j) { (cons x xs) -> xs j }
}

-- * Stream processors

cofun SP : -(A : Set) -> ++(B : Set) -> -(i : Size) -> +(j : Size) -> |i,j| -> Set
{ SP A B i j = Either ([j' < j] & (A -> SP A B i j'))
                      (B & ([i' < i] -> SP A B i' #))
}
pattern get j f  = left  (j , f)
pattern put b sp = right (b , sp)

cofun run : [A, B : Set] -> [i, j : Size] -> |i,j| -> SP A B i j -> Stream A # -> Stream B i
{ run A B i j (get j' f) as          = run A B i j' (f (head A as)) (tail A as)
; run A B i j (put b sp) as (i' < i) = cons b (run A B i' # (sp i') as)
}

{- Representing the Agda definition

  data SP (A B : Set) : Set where
    get : (A -> SP A B) -> SP AB
    put : B -> co (SP A B) -> SP AB

-}

cofun SP' : -(A : Set)  -> +(B : Set)  ->
           +(i : Size) -> -(j : Size) -> |j,i| -> Set
{ SP' A B i j = Either (A -> [i' < i] & SP' A B i' j)
                       (B & ([j' < j] -> SP' A B # j'))
--                     (B &  [j' < j] -> SP' A B # j') -- Parse error
}


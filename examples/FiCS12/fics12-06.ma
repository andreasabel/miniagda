-- 2012-02-05 Stream processors as a mixed coinductive-inductive type

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

-- * cartesian product

data Prod ++(A, B : Set) { pair (fst : A) (snd : B) }
fields fst, snd

-- * use streams of previous example

cofun Stream : ++(A : Set) -> -(i : Size) -> Set
{ Stream A i = [j < i] -> A & Stream A j
}
pattern cons x xs = x, xs

-- * Infinity streams

fun inst : [A : Set] [i : Size] (f : [j < $i] -> Stream A j) -> Stream A i
{ inst A i f j = (f $j) j }

let head [A : Set] [i : Size] (s : Stream A $i) : A
  = case s i { (cons a as) -> a }

let tail [A : Set] [i : Size] (s : Stream A $i) : Stream A i
  = case s i { (cons a as) -> as }


-- * Stream processors

{- Representing the Agda definition

  data SP (A B : Set) : Set where
    get : (A -> SP A B) -> SP AB
    put : B -> co (SP A B) -> SP AB

-}

cofun SP : -(A : Set) +(B : Set) -(i : Size) +(j : Size) -> |i,j| -> Set
{ SP A B i j = Either     (A -> [j' < j] &  SP A B i j')
                          (B & ([i' < i] -> SP A B i' #))
}
pattern get f       = left  f
pattern put b sp    = right (b , sp)

cofun run : [A, B : Set] [i, j : Size] -> |i,j| ->
  SP A B i j -> Stream A # -> Stream B i
{ run A B i j (get f)    as = case f (head A # as)
    { (j', sp) -> run A B i j' sp (tail A # as) }
; run A B i j (put b sp) as = \ i' -> (b, run A B i' # (sp i') as)
}

{-
-- Annoying:
-- need to force stream separately for head and tail

let Stream' ++(A : Set) = [i < #] -> Stream A i

let head [A : Set] (s : Stream' A) : A
  = fst (s 0)

let tail [A : Set] (s : Stream' A) : Stream' A
  = \ i -> snd (s $i) i

cofun run : [A, B : Set] [i, j : Size] -> |i,j| ->
  SP A B i j -> Stream' A -> Stream B i
{ run A B i j (get f) s = case f (head A s)
    { (j', sp) -> run A B i j' sp (tail A s) }
; run A B i j (put b sp) as = cons b (\ i' -> run A B i' # (sp i') as)
}
-}

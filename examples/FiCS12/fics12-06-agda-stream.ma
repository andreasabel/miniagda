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

data Prod ++(A, B : Set) { pair (fst : A) (snd : B) }
fields fst, snd

-- * front streams defined recursively

cofun Stream : ++(A : Set) -> -(i : Size) -> Set
{ Stream A i = Prod A ([j < i] -> Stream A j)
}
pattern cons x xs = pair x xs

-- * Infinity streams

cofun inst : [A : Set] [i : Size] (f : [j < $i] -> Stream A j) -> Stream A i
{ inst A i f = cons (fst (f 0)) (\ j -> snd (f $j) j) }

check
fun head : [A : Set] [i : Size] (s : Stream A i) -> A
{ head A i (cons a as) = a
}

check
fun tail : [A : Set] [i : Size] (s : Stream A $i) -> Stream A i
{ tail A i (cons a as) = inst A i as
}

-- * Stream processors

{- Representing the Agda definition

  data SP (A B : Set) : Set where
    get : (A -> SP A B) -> SP AB
    put : B -> co (SP A B) -> SP AB

-}

cofun SP : -(A : Set)  -> +(B : Set)  ->
           -(i : Size) -> +(j : Size) -> |i,j| -> Set
{ SP A B i j = Either (A -> [j' < j] &  SP A B i j')
                      (B & ([i' < i] -> SP A B i' #))
}
pattern get f    = left  f
pattern put b sp = right (b , sp)

check
cofun run : [A, B : Set] [i, j : Size] -> |i,j| ->
  SP A B i j -> Stream A # -> Stream B i
{ run A B i j (get f) (cons a as) = case f a
    { (j', sp) -> run A B i j' sp (inst A # as) }
; run A B i j (put b sp) as = cons b (\ i' -> run A B i' # (sp i') as)
}

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

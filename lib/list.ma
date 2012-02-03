-- list.ma -- MiniAgda list library

cofun List : ++(A : Set) -> +(i : Size) -> Set
{ List A i = Maybe (A & [i' < i] & List A i') 
}
pattern nil      = nothing
pattern cons a l = just (a, l) 

let consL [A : Set] [i : Size] (a : A) (as : List A i) : List A $i
  = cons a (i, as)

-- foldr

fun foldr : [A : Set] -> [B : Size -> Set] -> 
  ([i : Size] -> A -> [j < i] -> B j -> B i) ->
  ([i : Size] -> B i) -> 
  [i : Size] -> List A i -> B i
{ foldr A B f b i nil                = b i
; foldr A B f b i (cons a (j<i, as)) = f i a j (foldr A B f b j as)
}

-- map

check
let mapList : [A, B : Set] -> (A -> B) -> [i : Size] -> List A i -> List B i
  = \ A B f -> foldr A (List B) 
       (\ i a j bs -> cons (f a) (j, bs))   
       (\ i -> nil)

fun mapList : [A, B : Set] -> (A -> B) -> [i : Size] -> List A i -> List B i
{ mapList A B f i nil = nil
; mapList A B f i (cons a (j, as)) = cons (f a) (j, mapList A B f j as)
}

-- append

check
let append : [A : Set] -> 
             [i : Size] -> List A i -> 
             [j : Size] -> List A j -> List A (i+j)
  = \ A i as j bs -> 
      foldr A (\ i -> List A (i+j)) 
        (\ i b i' bs -> cons b (i'+j, bs)) 
        (\ i -> bs) 
        i 
        as

fun append : [A : Set] -> 
             [i : Size] -> |i| -> List A i -> 
             [j : Size] -> List A j -> List A (i+j)
{ append A i nil                 j bs = bs
; append A i (cons a (i'<i, as)) j bs = cons a (i'+j, append A i' as j bs)
}

-- drop

fun drop : [A : Set ] -> Nat # -> 
           [j : Size] -> List A j -> List A j
{ drop A zero j l                     = l
; drop A n    j nil                   = nil
; drop A n    j (cons a (j' < j, as)) = drop A (pred # n) j' as
}

-- take for lists is take for colists after embedding

-- fold left

check
fun foldl : [A, B : Set] -> (B -> A -> B) -> B ->
            [i : Size] -> List A i -> B
{ foldl A B f acc i nil = acc
; foldl A B f acc i (cons a (j, as)) = foldl A B f (f acc a) j as
}

-- fold left from fold right

let foldl' : [A : Set] -> [B : Set] -> (B -> A -> B) -> 
  [i : Size] -> List A i -> B -> B
  = \ A B f -> foldr A (\ i -> B -> B) 
      (\ i a j r acc -> r (f acc a))
      (\ i acc -> acc)

let foldl : [A : Set] -> [B : Set] -> (B -> A -> B) -> B ->
  [i : Size] -> List A i -> B
  = \ A B f b i l -> foldl' A B f i l b      

-- reverse

let revApp [A : Set] (as : List A #) (bs : List A #) : List A # 
  = foldl A (List A #) (\ as a -> consL A # a as) bs # as

let reverse [A : Set] (as : List A #) : List A #
  = revApp A as nil


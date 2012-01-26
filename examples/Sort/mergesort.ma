sized data List (A : Set) : Size -> Set 
{ nil  : [i : Size] -> List A $i
; cons : [i : Size] -> A -> List A i -> List A $i
}

-- split in CPS 
fun split : [A : Set] -> [i : Size] -> List A i -> 
  (C : Set) -> (List A i -> List A i -> C) -> C
{ split A i (nil (j < i)) C k = k (nil j) (nil j)
; split A i (cons (j < i) a as) C k = 
    split A j as C (\ xs ys -> k ys (cons j a xs))
}

let Cmp : Set -> Set 1
  = \ A -> A -> A -> (C : Set) -> C -> C -> C
 
fun merge : [A : Set] -> (cmp : Cmp A) ->
  [i : Size] -> List A i ->
  [j : Size] -> List A j -> List A #
{ merge A cmp i (nil (k < i)) j ys = ys
; merge A cmp i xs j (nil (l < j)) = xs
; merge A cmp i (cons (k < i) a as) j (cons (l < j) b bs) =
    cmp a b (List A #) 
      (cons # a (merge A cmp k as $l (cons l b bs)))
      (cons # b (merge A cmp i (cons k a as) l bs))
}

fun mergesort : [A : Set] -> (cmp : Cmp A) -> [i : Size] -> List A i -> List A #
{ mergesort A cmp i (nil (j < i)) = nil j
; mergesort A cmp i (cons (j < i) a (nil (k < j))) = cons j a (nil k)
; mergesort A cmp i (cons (j < i) a (cons (k < j) b bs)) =
    split A k bs (List A #) (\ xs ys -> merge A cmp 
      # (mergesort A cmp $k (cons k a xs)) 
      # (mergesort A cmp $k (cons k b ys)))
} 

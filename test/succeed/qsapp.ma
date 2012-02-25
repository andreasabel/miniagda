-- 2010-06-21 Andreas Abel  
-- Quicksort (implementation using partition) in MiniAgda

-- Booleans

data Bool : Set
{ true : Bool
; false : Bool
}

fun if : [A : Set] -> Bool -> A -> A -> A
{ if A true  t e = t
; if A false t e = e
}

-- Natural numbers

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

fun leq : Nat -> Nat -> Bool 
{ leq  zero     n       = true
; leq (succ m)  zero    = false
; leq (succ m) (succ n) = leq m n
}

-- Lists over natural numbers as a sized inductive type

sized data List : Size -> Set
{ nil  : [i : Size] -> List ($ i) 
; cons : [i : Size] -> Nat -> List i -> List ($ i)
}

-- Partition a list, continuation-style
-- the lists passed to the continuation k are at most as big as the input list

fun partition : (Nat -> Bool) -> [i : Size] -> List i -> 
  [A : Set] -> (List i -> List i -> A) -> A
{ partition p i (nil  (i > j))     A k = k (nil j) (nil j)
; partition p i (cons (i > j) n l) A k = if A (p n)
   (partition p j l A (\ l1 -> \ l2 -> k (cons j n l1) l2)) -- then 
   (partition p j l A (\ l1 -> \ l2 -> k l1 (cons j n l2))) -- else
}

-- Quicksort-append
-- qsapp i l1 l2 = append (sort l1) l2

fun qsapp : [i : Size] -> List i -> List # -> List #
{ qsapp i (nil (i > j))      acc = acc
; qsapp i (cons (i > j) n l) acc = partition (\ m -> leq m n) j l (List #)
    (\ l1 -> \ l2 -> qsapp j l1 (cons # n (qsapp j l2 acc)))
}

-- Quicksort 

let quicksort : List # -> List # = \ l -> qsapp # l (nil #)

-- Testing

let n0 : Nat = zero
let n1 : Nat = succ n0
let n2 : Nat = succ n1
let n3 : Nat = succ n2
let n4 : Nat = succ n3
let n5 : Nat = succ n4
let n6 : Nat = succ n5
let n7 : Nat = succ n6
let n8 : Nat = succ n7
let n9 : Nat = succ n8

-- qsapp is fast enough even with MiniAgda CBN
let l : List # = 
  (cons # n4 (cons # n9 (cons # n1 (cons # n7 (cons # n6 
  (cons # n4 (cons # n0 (cons # n0 
  (cons # n3 (cons # n3 (cons # n3 (cons # n2 (cons # n3 (nil #))))))))))))))
-- eval  -- 2012-02-25 NO LONGER 
let l' : List # = quicksort l
 
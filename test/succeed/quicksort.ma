-- 2010-06-21 Andreas Abel
-- Quicksort (implementation using partition) in MiniAgda
-- more efficient implementation see qsapp.ma

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

-- Append lists (yields no size information)

fun append : List # -> List # -> List #
{ append (nil .#)       l = l
; append (cons .# x xs) l = cons # x (append xs l)
}

-- Partition a list, continuation-style

fun partition : (Nat -> Bool) -> [i : Size] -> List i ->
  [A : Set] -> (List i -> List i -> A) -> A
{ partition p i (nil  (i > j))     A k = k (nil j) (nil j)
; partition p i (cons (i > j) n l) A k = if A (p n)
   (partition p j l A (\ l1 -> \ l2 -> k (cons j n l1) l2)) -- then
   (partition p j l A (\ l1 -> \ l2 -> k l1 (cons j n l2))) -- else
}

-- Quicksort

fun quicksort : [i : Size] -> List i -> List #
{ quicksort i (nil  (i > j))     = nil j
; quicksort i (cons (i > j) n l) = partition (\ m -> leq m n) j l (List #)
    (\ l1 -> \ l2 -> append (quicksort j l1) (cons # n (quicksort j l2)))
}

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

{- MiniAgda CBN is too inefficient to do this in reasonable time
let l : List # =
  (cons # 4 (cons # 9 (cons # 1 (cons # 7 (cons # 6
  (cons # 4 (cons # 0 (cons # 0
  (cons # 3 (cons # 3 (cons # 3 (cons # 2 (cons # 3 (nil #))))))))))))))
-}
let l : List # = cons # n1 (cons # n3 (cons # n0 (cons # n2 (nil #))))
eval let l' : List # = quicksort # l

-- 2010-06-21 Andreas Abel  
-- Quicksort (naive implementation using filter) in MiniAgda

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

-- Filter a list (the output list is as most as long as the input list)

fun filter : (Nat -> Bool) -> [i : Size] -> List i -> List i
{ filter p i (nil  (i > j))     = nil j
; filter p i (cons (i > j) n l) = if (List ($ j)) (p n)
   (cons j n (filter p j l)) -- then
   (filter p j l)            -- else
}

-- Quicksort 

fun quicksort : [i : Size] -> List i -> List #
{ quicksort i (nil (i > j))      = nil j
; quicksort i (cons (i > j) n l) = 
      append (quicksort j (filter (\ m -> leq m n) j l)) 
    (cons # n (quicksort j (filter (leq (succ n)) j l))) 
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
 
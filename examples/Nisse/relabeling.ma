-- 2010-04-28 Andreas Abel
-- breadth-first relabeling of possibly infinite trees
-- (Jones and Gibbons, 1993)

data Prod (+ A : Set)(+ B : Set) : Set 
{ pair : (fst : A) -> (snd : B) -> Prod A B
}

sized codata Stream (+ A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A ($ i)
}

sized codata Tree (+ A : Set) : Size -> Set 
{ leaf : [i : Size] -> Tree A ($ i)
; node : [i : Size] -> A -> Tree A i -> Tree A i -> Tree A ($ i)
}

{- problematic, since product as return type 
cofun lab : (i : Size) -> [A : Set] -> [B : Set] ->
   Tree A i -> Stream (Stream B #) i -> 
   Prod (Tree B i) (Stream (Stream B #) i)
{ lab ($ i) A B (leaf .A .i) bss = 
    pair (Tree B ($ i)) (Stream (Stream B #) i) (leaf B i) bss
; lab ($ i) A B (node .A .i l x r) 
    (cons .(Stream B #) .i (cons .B .# b bs) bss) =
    pair (Tree B ($ i)) (Stream (Stream B #) ($ i))
     (node B i (fst (Tree B i) (Stream (Stream B #) i) (lab i A B l bss)) b 
               (fst (Tree B i) (Stream (Stream B #) i) (lab i A B r 
               (snd (Tree B i) (Stream (Stream B #) i) (lab i A B l bss)))))
     (cons (Stream B #) i bs 
               (snd (Tree B i) (Stream (Stream B #) i) (lab i A B r 
               (snd (Tree B i) (Stream (Stream B #) (lab i A B l bss)))))) 
}
-}

cofun lab2 : [i : Size] -> [A : Set] -> [B : Set] ->
   Tree A i -> Stream (Stream B #) i -> Stream (Stream B #) i
{ lab2 ($ i) A B (leaf .i) bss = bss
; lab2 ($ i) A B (node .i x l r) (cons .i (cons .# b bs) bss) =
      cons i bs (lab2 i A B r (lab2 i A B l bss))
}

cofun lab1 : [i : Size] -> [A : Set] -> [B : Set] ->
   Tree A i -> Stream (Stream B #) i -> Tree B i
{ lab1 ($ i) A B (leaf .i)       bss = leaf i
; lab1 ($ i) A B (node .i x l r) 
    (cons  .i (cons .# b bs) bss) =
     (node i b (lab1 i A B l bss) 
               (lab1 i A B r (lab2 i A B l bss)))
}

-- this auxiliary function replaces the original circular program
cofun label2 : [i : Size] -> [A : Set] -> [B : Set] -> 
  Tree A i -> Stream B # -> Stream (Stream B #) i 
{ label2 ($ i) A B t bs = lab2 ($ i) A B t 
    (cons i bs (label2 i A B t bs))
}

-- main program
cofun label : [i : Size] -> [A : Set] -> [B : Set] -> 
  Tree A i -> Stream B # -> Tree B i
{ label i A B t bs = lab1 i A B t (cons i bs (label2 i A B t bs))
}

-- testing...

data Unit : Set
{ unit : Unit
}

data Nat : Set 
{ Z : Nat
; S : Nat -> Nat
}

cofun nats : [i : Size] -> Nat -> Stream Nat i
{ nats ($ i) n = cons i n (nats i (S n))
}

fun fib : Nat -> Tree Unit #
{ fib Z         = leaf #
; fib (S Z)     = leaf #
; fib (S (S n)) = node # unit (fib n) (fib (S n))
}

cofun fibTree : [i : Size] -> Nat -> Tree Unit i
{ fibTree ($ i) n = node i unit (fib n) (fibTree i (S n))
}

-- UNREADABLE OUTPUT
-- eval let t : Tree Nat # = label # Unit Nat (fibTree # Z) (nats # Z)

cofun infTree : [i : Size] -> Tree Unit i
{ infTree ($ i) = node i unit (infTree i) (infTree i)
}

-- HARDLY READABLE
-- eval let t' : Tree Nat # = label # Unit Nat (infTree #) (nats # Z)

fun finTree : Nat -> Tree Unit #
{ finTree Z = leaf #
; finTree (S n) = node # unit (finTree n) (finTree n)
}

eval let t'' : Tree Nat # = label # Unit Nat (finTree (S (S (S Z)))) (nats # Z)









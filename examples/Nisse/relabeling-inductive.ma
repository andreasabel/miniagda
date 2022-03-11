-- 2010-05-20 Andreas Abel
-- breadth-first relabeling of finite trees
-- (Jones and Gibbons, 1993)

data Prod (+ A : Set)(+ B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B
}
fields fst, snd

sized codata Stream (+ A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A ($ i)
}
fields head, tail

sized data Tree (+ A : Set) : Size -> Set
{ leaf : [i : Size] -> Tree A ($ i)
; node : [i : Size] -> A -> Tree A i -> Tree A i -> Tree A ($ i)
}

-- this definition is fine since the result type is a product
-- where each of its components is coinductive in i (TLCA, 2003)
fun lab : [i : Size] -> [A : Set] -> [B : Set] ->
   Tree A i -> Stream (Stream B #) i ->
   Prod (Tree B i) (Stream (Stream B #) i)
{ lab j A B (leaf (j > i)) bss =
    pair {- (Tree B ($ i)) (Stream (Stream B #) ($ i)) -} (leaf i) bss
; lab j A B (node (j > i) x l r)
    (cons  .i (cons .# b bs) bss) =
    pair {- (Tree B ($ i)) (Stream (Stream B #) ($ i)) -}
     (node i b (fst (Tree B i) (Stream (Stream B #) i) (lab i A B l bss))
               (fst (Tree B i) (Stream (Stream B #) i) (lab i A B r
               (snd (Tree B i) (Stream (Stream B #) i) (lab i A B l bss)))))
     (cons i bs
               (snd (Tree B i) (Stream (Stream B #) i) (lab i A B r
               (snd (Tree B i) (Stream (Stream B #) i) (lab i A B l bss)))))
}


-- this auxiliary function replaces the original circular program
cofun label2 : [i : Size] -> [A : Set] -> [B : Set] ->
  Tree A i -> Stream B # -> Stream (Stream B #) i
{ label2 ($ i) A B t bs = snd (Tree B ($ i)) (Stream (Stream B #) ($ i))
    (lab ($ i) A B t (cons i bs (label2 i A B t bs)))
}

-- main program
fun label : [i : Size] -> [A : Set] -> [B : Set] ->
  Tree A i -> Stream B # -> Tree B i
{ label i A B t bs = fst (Tree B i) (Stream (Stream B #) i)
   (lab i A B t (cons i bs (label2 i A B t bs)))
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

eval let t0 : Tree Nat # = label # Unit Nat (finTree Z) (nats # Z)
eval let t1 : Tree Nat # = label # Unit Nat (finTree (S Z)) (nats # Z)
eval let t2 : Tree Nat # = label # Unit Nat (finTree (S (S Z))) (nats # Z)
eval let t3 : Tree Nat # = label # Unit Nat (finTree (S (S (S Z)))) (nats # Z)

{- evaluation of t2

t2 = label (fintree 2) (nats 0)
   = fst (lab (fintree 2) (cons (nats 0) (label2 (fintree 2) (nats 0))))
   = fst (lab (node u (ft 1) (ft 1)) (cons (cons 0 (nats 1)) (label2 ... )
   = fst (pair (node 0 (fst (lab (ft 1) (label2 ...)))
                       (fst (lab (ft 1) (snd (lab (ft 1) (label2 ...))))))
               (cons (nats 1) (snd (lab (ft 1) (snd (lab (ft 1) (label2 ...)))))))
   = node 0 (fst (lab (ft 1) (label2 ...)))
            (fst (lab (ft 1) (snd (lab (ft 1) (label2 ...))))))

-}


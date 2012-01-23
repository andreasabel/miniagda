-- 2010-05-20, -06-08 Andreas Abel
-- breadth-first relabeling of possibly infinite trees (Jones and Gibbons, 1993)
-- see Nils Anders Danielsson, Beating the Productivity Checker (PAR 2010, FLoC)
-- 2012-01-22 parameters gone from constructors

data Prod (+ A : Set)(+ B : Set) : Set 
{ pair : (fst : A) -> (snd : B) -> Prod A B
} fields fst, snd

sized codata Stream (+ A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A ($ i)
} fields head, tail

sized codata Tree (+ A : Set) : Size -> Set 
{ leaf : [i : Size] -> Tree A ($ i)
; node : [i : Size] -> A -> Tree A i -> Tree A i -> Tree A ($ i)
}

-- this definition is fine since the result type is a product
-- where each of its components is coinductive in i (TLCA, 2003)
cofun lab : [i : Size] -> [A : Set] -> [B : Set] ->
   Tree A i -> Stream (Stream B #) i -> 
   Prod (Tree B i) (Stream (Stream B #) i)
{
  lab ($ i) A B (leaf {-.A-} .i) bss = 
    pair {- (Tree B ($ i)) (Stream (Stream B #) ($ i)) -} (leaf {-B-} i) bss

; lab ($ i) A B (node {-.A-} .i x l r) 
    (cons {- .(Stream B #) -} .i (cons {-.B-} .# b bs) bss) =

      -- recursive call on left subtree
      let    pl   : Prod (Tree B i) (Stream (Stream B #) i)
                  = lab i A B l bss 

      -- recursive call on right subtree, threading the label stream-stream
      in let pr   : Prod (Tree B i) (Stream (Stream B #) i)
                  = lab i A B r (snd (Tree B i) (Stream (Stream B #) i) pl) 

      in pair {- (Tree B ($ i)) (Stream (Stream B #) ($ i)) -}
           (node {-B-} i b (fst (Tree B i) (Stream (Stream B #) i) pl)
                       (fst (Tree B i) (Stream (Stream B #) i) pr))
           (cons {- (Stream B #) -} i bs 
                       (snd (Tree B i) (Stream (Stream B #) i) pr))
}


-- this auxiliary function replaces the original circular program
cofun label2 : [i : Size] -> [A : Set] -> [B : Set] -> 
  Tree A i -> Stream B # -> Stream (Stream B #) i 
{ label2 ($ i) A B t bs = snd (Tree B ($ i)) (Stream (Stream B #) ($ i))
    (lab ($ i) A B t (cons {- (Stream B #)-} i bs (label2 i A B t bs)))
}

-- main program
fun label : [i : Size] -> [A : Set] -> [B : Set] -> 
  Tree A i -> Stream B # -> Tree B i
{ label i A B t bs = fst (Tree B i) (Stream (Stream B #) i)
   (lab i A B t (cons {-(Stream B #)-} i bs (label2 i A B t bs)))
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
{ nats ($ i) n = cons {-Nat-} i n (nats i (S n))
}

fun finTree : Nat -> Tree Unit #
{ finTree Z = leaf {- Unit -} #
; finTree (S n) = node {- Unit -} # unit (finTree n) (finTree n)
}

eval let t0 : Tree Nat # = label # Unit Nat (finTree Z) (nats # Z)
eval let t1 : Tree Nat # = label # Unit Nat (finTree (S Z)) (nats # Z)
eval let t2 : Tree Nat # = label # Unit Nat (finTree (S (S Z))) (nats # Z)
eval let t3 : Tree Nat # = label # Unit Nat (finTree (S (S (S Z)))) (nats # Z)





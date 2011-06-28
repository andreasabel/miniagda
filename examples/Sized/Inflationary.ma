-- 2011-03-13
-- copied to projects/bp_papers/sized/coinductive/

-- inflationary (^+) data declaration

data Nat ^+(i : Size) : Set 
{ zero : [j : Size] -> |j| < |i| -> Nat i
; succ : [j : Size] -> |j| < |i| -> Nat j -> Nat i
}

data Ex (P : Size -> Set) : Set
{ ex : (i : Size) -> P i -> Ex P
}

let NAT : Set = Ex Nat

fun inc : NAT -> NAT
{ inc (ex i n) = ex ($i) (succ i n)
}
let ZERO : NAT = ex $0 (zero 0)
let ONE  : NAT = inc ZERO

fun plus0 : [i : Size] -> Nat i -> [j : Size] -> Nat j -> NAT
{ plus0 i (zero i')    j m = ex j m
; plus0 i (succ i' n') j m = inc (plus0 i' n' j m)
}

fun plus' : [i : Size] -> Nat i -> NAT -> NAT
{ plus' i (zero i')    m = m
; plus' i (succ i' n') m = inc (plus' i' n' m)
}

fun plus : NAT -> NAT -> NAT
{ plus (ex i n) m = plus' i n m 
}

fun pred : [i : Size] -> Nat i -> Nat i
{ pred i (zero j  ) = zero j
; pred i (succ j n) = n
}

fun PRED : NAT -> NAT
{ PRED (ex i n) = ex i (pred i n)
}  

fun minus' : [i : Size] -> Nat i -> [j : Size] -> Nat j -> Nat i
{ minus i n j (zero j')   = n
; minus i n j (succ j' m) = minus i (pred i n) j' m
}

fun minus : [i : Size] -> Nat i -> NAT -> Nat i
{ minus i n (ex j m) = minus i n j m
}

-- div' n m = ceil(n/m+1)
fun div' : [i : Size] -> Nat i -> NAT -> Nat i
{ div' i (zero j)   m = zero j
; div' i (succ j n) m = succ j (div' j (minus j n m) m)
}

-- sup : Size -> Size -> Size
fun max : [i : Size] -> Nat i -> Nat i -> Nat i
{ max i (zero j)   m           = m
; max i (succ j n) (zero j')   = succ j n
; max i (succ j n) (succ j' m) = succ (sup j j') (max (sup j j') n m)
}

-- deflationary (^-) codata declaration

codata Stream (A : Set) ^-(i : Size) : Set 
{ cons : (head : [j : Size] -> |j| < |i| -> A) -> 
         (tail : [j : Size] -> |j| < |i| -> Stream A j) -> Stream A i
} fields head, tail

cofun map : [A, B : Set] -> (f : A -> B) ->
            [i : Size] -> Stream A i -> Stream B i
{ head j (map A B f i s) = f (head j s)
; tail j (map A B f i s) = map A B f j (tail j s)
}

cofun upfrom : [i : Size] -> NAT -> Stream NAT i
{ head j (upfrom i n) = n
; tail j (upfrom i n) = upfrom j (inc n)
}

cofun nats : [i : Size] -> Stream (Nat i) i
{ head j (nats i) = zero j
; tail j (nats i) = map (Nat j) (Nat i) (succ j) j (nats j)
}

cofun zipWith : [A, B, C : Set] -> (f : A -> B -> C) ->
  [i : Size] -> Stream A i -> Stream B i -> Stream C i
{ head j (zipWith A B C f i as bs) = f (head j as) (head j bs)
; tail j (zipWith A B C f i as bs) = zipWith A B C f j (tail j as) (tail j bs)
}

let cons- : [A : Set] -> [i : Size] -> A -> Stream A i -> Stream A i
  = \ A i a s -> cons (\ j -> a) (\ j -> s)

cofun fib : [i : Size] -> Stream NAT i
{ head j (fib i) = ZERO
; tail j (fib i) = zipWith NAT NAT NAT plus (fib j) (cons- ONE (fib j))
}

cofun psum : [i : Size] -> Stream NAT i -> Stream NAT i
{ head j (psum i s) = head j s
; tail j (psum i s) = zipWith NAT NAT NAT plus j (tail j s) (psum j s)
}

{- functional version not memoizing!
psum : (nat -> nat) -> nat -> nat
psum s 0       = s 0
psum s (n + 1) = s (n + 1) + psum s n
-}

let STREAM : Set -> Set
  = \ A -> [i : Size] -> Stream A i

let TAIL : [A : Set] -> STREAM A -> STREAM A
 = \ A s i -> tail i (s $i)

fun drop : [A : Set] -> [i : Size] -> Nat i -> STREAM A -> STREAM A
{ drop A i (zero j)   s = s
; drop A i (succ j n) s = drop A j n (TAIL s)
}

-- negative types

data Tm (i : Size) : Set 
{ abs : [j : Size] -> |j| < |i| -> (Tm j -> Tm j) -> Tm i
}

let TM : Set = Ex Tm

fun app : TM -> TM -> TM
{ app (ex i (abs j t)) (ex k u) = ex ? (t u)  -- t : Tm j -> Tm j 
                                              -- u : Tm k
} 

fun appSQ : [i : Size] -> Tm i -> [C : Set] -> 
            ([j : Size] -> |j| < |i| -> (Tm j -> Tm j) -> C) -> C
{ app i (abs j t) C k = k j t
}

-- HOAS

data Tm ^+(i : Size) : Set
{ app : [j : Size] -> |j| < |i| -> Tm j -> Tm j -> Tm i
; abs : [j : Size] -> |j| < |i| ->
        ([k : Size] -> Tm k -> Tm (k + j)) -> Tm i
}

fun cp : [i : Size] -> Tm i -> Tm i
{ cp i (app j t u) = app j (cp j t) (cp j u)
; cp i (abs j f)   = abs j (\ k x -> cp (k + j) (f k x)) -- not ok! 
}

let TM : Set = Ex Tm

data Red : [i : Size] -> Tm i -> [j : Size] -> Tm j -> Set  
{ beta : [i, j : Size] -> 
         (t : [k : Size] -> Tm k -> Tm (k + j)) ->
         (u : Tm i) ->
         Red ($i)
             (app i (abs j t) u : Tm $i)
             (i + j) 
             (t i u : Tm (i + j))
} 

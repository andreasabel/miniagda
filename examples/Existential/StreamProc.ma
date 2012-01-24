-- 2012-01-23 Stream processors

-- * prelude

record Unit : Set
{ unit : Unit
}

-- * Booleans

data Bool : Set 
{ true  : Bool
; false : Bool
}

fun If : Bool -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

-- * disjoint sum

let Either : ++(A, B : Set) -> Set
  = \ A B -> (b : Bool) & If b A B

pattern left  a = (true  , a)
pattern right b = (false , b)

-- * lazy pair

record Prod ++(A, B : Set) : Set 
{ pair : (fst : A) -> (snd : B) -> Prod A B
} fields fst, snd

cofun mapProd : [A1, A2 : Set] -> (A1 -> A2) -> 
                [B1, B2 : Set] -> (B1 -> B2) -> Prod A1 B1 -> Prod A2 B2
{ mapProd A1 A2 f B1 B2 g p .fst = f (p .fst)
; mapProd A1 A2 f B1 B2 g p .snd = g (p .snd)
}

cofun mapFst : [A1, A2 : Set] -> (A1 -> A2) -> 
               [B : Set] -> Prod A1 B -> Prod A2 B
{ mapFst A1 A2 f B p .fst = f (p .fst)
; mapFst A1 A2 f B p .snd = p .snd
}

cofun mapSnd : [A, B1, B2 : Set] -> (B1 -> B2) -> Prod A B1 -> Prod A B2
{ mapSnd A B1 B2 g p .fst = p .fst
; mapSnd A B1 B2 g p .snd = g (p .snd)
}

-- * streams defined recursively

cofun Stream : ++(A : Set) -> -(i : Size) -> Set
{ Stream A i = [j < i] -> Prod A (Stream A j)
}

pattern cons x xs = pair x xs

{-
cofun build : [A, S : Set] -> (S -> Prod A S) -> [i : Size] -> S -> Stream A i
{ build A S step i start (j < i) .fst = step start .fst
; build A S step i start (j < i) .snd = build A S step j (step start .snd) 
}
-}

cofun build : [A : Set] -> [S : -Size -> Set] -> 
  ([j : Size] -> S $j -> Prod A (S j)) -> [i : Size] -> S i -> Stream A i
{ build A S step i start (j < i) .fst = step j start .fst
; build A S step i start (j < i) .snd = build A S step j (step j start .snd) 
}

let map : [A, B : Set] -> (A -> B) -> [i : Size] -> Stream A i -> Stream B i
 = \ A B f -> build B (Stream A) (\ j as -> mapFst A B f (Stream A j) (as j))
--      (\ j as -> mapProd A B f (Stream A j) (Stream A j) (\ as -> as) (as j))

let repeat : [A : Set] -> A -> [i : Size] -> Stream A i
  = \ A a i -> build A (\ j -> Unit) (\ j u -> pair a u) i unit

cofun repeat' : [A : Set] -> A -> [i : Size] -> Stream A i
{ repeat' A a i (j < i) .fst = a
; repeat' A a i (j < i) .snd = repeat' A a j
}

-- * Infinity streams

let head : [A : Set] -> Stream A # -> A
  = \ A s -> s 0 .fst

cofun tail' : [A : Set] -> [i : Size] -> Stream A $i -> Stream A i
{ tail' A i s = s i .snd
} 

let tail : [A : Set] -> Stream A # -> Stream A #
  = \ A s -> tail' A # s

{- BUG j < # not handled correctly!
cofun tail_ : [A : Set] -> Stream A # -> Stream A #
{ tail_ A s j = s $j .snd
} 
-}

let split' : [A : Set] -> [i : Size] -> Stream A $i -> Prod A (Stream A i)
 = \ A i s -> s i

let split : [A : Set] -> Stream A # -> Prod A (Stream A #)
  = \ A s -> split' A # s

-- CHECK FOR omega-instantiation not needed??



-- * Stream processors

fun A : Set {}
fun B : Set {}

cofun SP' : ++(X : Set) -> +(i : Size) -> Set
{ SP' X i = [j < i] & Either (A -> SP' X j) X
} 

pattern get f = left f
pattern out x = right x

cofun SP : -(i : Size) -> Set
{ SP i = [j < i] -> Prod B (SP' (SP j) #)
}

pattern put b sp = pair b sp

fun run' : [i : Size] -> (SP i -> Stream A # -> Stream B i) ->
           [j : Size] -> SP' (SP i) j -> Stream A # -> Stream B i
{ run' i r j (k < j , get f)  as = run' i r k (f (head A as)) (tail A as)
; run' i r j (k < j , out sp) as = r sp as
}

cofun run : [i : Size] -> SP i -> Stream A # -> Stream B i
{ run i sp as (j < i) = mapSnd B (SP' (SP j) #) (Stream B j) 
   (\ sp -> run' j (run j) # sp as) 
   (sp j)

}


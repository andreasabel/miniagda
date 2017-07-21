
record Stream +(A : Set) -(i : Size)
{ cons (head : A) (tail : [j < i] -> Stream A j)
} fields head, tail

fun repeat : [A : Set] -> (a : A) -> [i : Size] -> Stream A i
{ repeat A a i .head         = a
; repeat A a i .tail (j < i) = repeat A a j
}

fun map : [A, B : Set] -> (f : A -> B) -> [i : Size] -> Stream A i -> Stream B i
{ map A B f i s .head = f (s .head)
; map A B f i s .tail (j < i) = map A B f j (s .tail j)
}

fun zipWith : [A, B, C : Set] -> (f : A -> B -> C) -> [i : Size] -> Stream A i -> Stream B i -> Stream C i
{ zipWith A B C f i as bs .head         = f (as .head) (bs .head)
; zipWith A B C f i as bs .tail (j < i) = zipWith A B C f j (as .tail j) (bs .tail j)
}

data Nat { zero; suc (pred : Nat) }

fun plus : Nat -> Nat -> Nat
{ plus zero m = m
; plus (suc n) m = suc (plus n m)
}

fun fib : [i : Size] -> Stream Nat i
{ fib i .head                       = zero
; fib i .tail (j < i) .head         = suc zero
; fib i .tail (j < i) .tail (k < j) =
   zipWith Nat Nat Nat plus k (fib k) (fib j .tail k)
}

{- no sizes

record Stream (A : Set)
{ cons (head : A) (tail : Stream A)
} fields head, tail

cofun repeat : [A : Set] -> (a : A) -> Stream A
{ repeat A a .head = a
; repeat A a .tail = repeat A a
}

-}

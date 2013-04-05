data Stream +(A : Set) -(i : Size) : Set
{ cons (head : [j < i] -> A) (tail : [j < i] -> Stream A j)
} fields head, tail

let cons_ [A : Set] [i : Size] (a : A) (as : Stream A i) : Stream A $i
  = cons (\ j -> a) (\ j -> as)

trustme
fun merge : [A : Set] -> [j : Size] -> [i < $$j] -> |i| -> Stream A i -> Stream A j -> Stream A i
{ merge A j i s t .head i' = s .head i'
; merge A j i s t .tail i' = merge A i' i' t (s .tail i')
}
-- have: i' < i < $$j ==> i <= $j , i' <= j
-- show: i' < $$i' ok
-- show: Stream A j <= Stream A i'  ok, since i' <= j

fun eta : [A : Set] -> [j : Size] -> ([k < j] -> Stream A $k) -> Stream A j
{ eta A j f .head k = f k .head k
; eta A j f .tail k = f k .tail k
}

-- Note: not SN
cofun consRepeat : [A : Set] -> (a : A) -> [j : Size] -> |j| -> Stream A j
{ consRepeat A a j = eta A j (\ k -> cons_ A k a (consRepeat A a k))
}

-- Note: not SN
check
cofun huttonRepeat : [A : Set] -> (a : A) -> [i : Size] -> |i| -> Stream A i
{ huttonRepeat A a i .head j = a
; huttonRepeat A a i .tail j = eta A j (\ k -> merge A k $k (huttonRepeat A a $k) (huttonRepeat A a $k .tail k))
}

fun huttonRepeat : [A : Set] -> (a : A) -> [i : Size] -> |i| -> Stream A i
{ huttonRepeat A a i .head j         = a
; huttonRepeat A a i .tail j .head k = merge A k $k (huttonRepeat A a $k) (huttonRepeat A a $k .tail k) .head k
; huttonRepeat A a i .tail j .tail k = merge A k $k (huttonRepeat A a $k) (huttonRepeat A a $k .tail k) .tail k
}

data Nat : Set { zero; suc (n : Nat) }

let one : Nat = suc zero

fun huttonOnes : [i : Size] -> |i| -> Stream Nat i
{ huttonOnes i .head j         = zero
; huttonOnes i. tail j .head k = merge Nat k $k (huttonOnes $k) (huttonOnes $k .tail k) .head k
; huttonOnes i. tail j .tail k = merge Nat k $k (huttonOnes $k) (huttonOnes $k .tail k) .tail k
}

cofun consZeros : [j : Size] -> |j| -> Stream Nat j
{ consZeros j = eta Nat j (\ k -> cons_ Nat k zero (consZeros k))
}

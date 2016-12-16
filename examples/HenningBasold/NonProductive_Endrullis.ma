
data Nat { zero ; succ (n : Nat) }

record Stream +(A : Set) -(i : Size)
{ cons (head : A) (tail : [j < i] -> Stream A j)
} fields head, tail

mutual {
fun evens : [A : Set] -> [i : Size] -> Stream A # -> Stream A i
{ evens A i s .head = s .head
; evens A i s .tail (j < i) = odds A j (s .tail #)
}

fun odds : [A : Set] -> [i : Size] -> Stream A # -> Stream A i
{ odds A i s .head = evens A i (s .tail #) .head
; odds A i s .tail (j < i) = evens A i (s .tail #) .tail j
}

} -- mutual


{-
fun evens : [A : Set] -> [i : Size] -> Stream A # -> Stream A i
{ evens A i s .head = s .head
; evens A i s .tail (j < i) = evens A j (s .tail # .tail #)
}

fun odds : [A : Set] -> [i : Size] -> Stream A # -> Stream A i
{ odds A i s .head = s .tail # .head
; odds A i s .tail (j < i) = evens A i (s .tail #) .tail j
}
-}


fun np : [i : Size] -> |i| -> Stream Nat i
{ np i .head = zero
; np i .tail j = evens Nat j (np #)
-- ; np i .tail j .head = succ zero
-- ; np i .tail j .tail k = evens Nat k (np #)
}

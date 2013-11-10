{- Challenge: Thue Morse Sequence

   tm = 0 : interleave (map not (dropHalf tm)) (tail tm)
   dropHalf (x : y : xs) = x : dropHalf xs
   interleave (x : xs) ys = x : interleave ys xs

-}

-- tm = 1 + (tm/2) + tm - 1
-- tm : S (3i/2)
-- tm = 0 : interleave (map not (dropHalf tm)) (tail tm)
-- tm (i + 1) = 0 : interleave (map not (dropHalf (tm i))) (tail (tm i))
-- LHS: 3i/2 + 3/2
-- RHS: 1 + 3i/4 + (3i/2 - 1)
-- dropHalf : S (2i) -> S i
-- dropHalf (x : y : xs) = x : dropHalf xs
-- interleave : S i -> S j -> S (i + j)
-- interleave (x : xs) ys = x : interleave ys xs

sized codata Stream +(A : Set) : -Size -> Set
{ cons [i : Size] (head : A) (tail : Stream A i) : Stream A $i
} fields head, tail

cofun dropHalf : [A : Set] [i : Size] (s : Stream A (i + i)) -> Stream A i
{ dropHalf A $i (cons .($i + i) x (cons .(i + i) y xs)) = cons i x xs
}

cofun interleave : [A : Set] [i : Size] (s : Stream A i) [j : Size] |i + j| (t : Stream A j) -> Stream A (i + j)
{ interleave A $i (cons .i x xs) j ys = cons (i + j) x (interleave A j ys i xs)
}

cofun map : [A, B : Set] [i : Size] (f : A -> B) (s : Stream A i) -> Stream B i
{ map A B $i f (cons .i x xs) = cons i (f x) (map A B i f xs)
}

data Bool { true; false }

fun not : Bool -> Bool
{ not true = false
; not false = true
}

coalg Stream +(A : Set) : -Size -> Set
{ head : [i : Size] -> Stream A $i -> A
; tail : [i : Size] -> Stream A $i -> Stream A i
}

cofun thueMorse : [i : Size] -> Stream Bool (i + i)
{        (head i   (thueMorse  .($i/2))) = false
  head i (tail .$i (thueMorse .($$i/2))) = true
  tail i (tail .$i (thueMorse .($$i/2))) =
    interleave Bool i (tail (thueMorse (i + 1 {- 0.5 -})))
                    i (map Bool Bool i not (dropHalf Bool i (tail (tail (thueMorse i)))))
  -- further unfolding
  head (tail (tail thueMorse)) = head (tail thueMorse)
  tail (tail (tail thueMorse)) =
    interleave (map not (dropHalf (tail (tail thueMorse))))
               (tail (tail thueMorse))
  -- even further
  head (tail (tail (tail thueMorse))) = not (head (tail (tail thueMorse)))
  tail (tail (tail (tail thueMorse))) =
    interleave (tail (tail thueMorse))
               (map not (dropHalf (tail (tail (tail (tail thueMorse))))))
}

cofun thueMorse : [i : Size] -> Stream Bool (i + i)
{ thueMorse $i = cons ($i + i) false (cons (i + i) true
  (interleave Bool i (tail (thueMorse (i + 1 {- 0.5 -})))
                   i (tail (map Bool Bool i not (dropHalf Bool i (thueMorse i))))))
}

{-
cofun thueMorse : [i : Size] -> Stream Bool (i + i)
{ thueMorse $i = cons ($i + i) false (interleave Bool $i (map Bool Bool $i not (dropHalf Bool (i + ???) (thueMorse (i + 0.5)))) i (thueMorse (i + 0.5) .tail))
}

  tm = 0 : interleave (map not (dropHalf tm)) (tail tm)
head tm = 0
head (tail tm) = not (head tm)
tail (tail tm) = interleave (tail tm) (tail (map not (dropHalf tm)))
-}

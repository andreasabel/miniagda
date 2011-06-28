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

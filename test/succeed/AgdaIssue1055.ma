-- 2014-02-17

data Nat { zero ; suc (n : Nat) }

-- The following function is accepted by the termination checker in
-- Agda-2.3.2.2, but it is rejected by the termination checker in
-- the current development version. (The function was adapted from Lee,
-- Jones, and Ben-Amram, POPL '01).
fun p : (m, n, r : Nat) -> Nat
{ p m n       (suc r) = p m r n
; p m (suc n) zero    = p zero n m
; p m zero    zero    = m
}
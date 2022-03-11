data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

sized data Ord : Size -> Set
{ ozero : [i : Size] -> Ord ($ i)
; osucc : [i : Size] -> Ord i -> Ord ($ i)
; olim  : [i : Size] -> (Nat -> Ord i) -> Ord ($ i)
}

fun maxO : [i : Size] -> Ord i -> Ord i -> Ord i
{ maxO i (ozero (i > j)) q = q
; maxO i p (ozero (i > k)) = p
; maxO i (olim (i > j) f) (olim (i > k) g) =
   olim (max j k) (\ n -> maxO (max j k) (f n) (g n))
; maxO i (osucc (i > j) p) (osucc (i > k) q) =
   osucc (max j k) (maxO (max j k) p q)
-- CANNOT DEFINE MISSION CLAUSES
}

fun idO : [i : Size] -> Ord i -> Ord i
{ idO i (ozero (i > j)  ) = ozero j
; idO i (osucc (i > j) p) = osucc j (idO j p)
; idO i (olim  (i > j) f) = olim  j (\ n -> idO j (f n))
}

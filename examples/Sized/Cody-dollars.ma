{- 2010-02-02 Cody Roux communicated and observation of Frederic
   Blanqui that the "non-linear" size-assignment for constructors (see
   M below) does not allow to express the precise sizes in a deep
   match involving a limit ordinal (see L below).  From this I could
   construct a non-looping term in MiniAgda -}


data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

sized data O : Size -> Set
{ Z : [i : Size] -> O ($ i)
; S : [i : Size] -> O i -> O ($ i)
; L : [i : Size] -> (Nat -> O i) -> O ($ i)
; M : [i : Size] -> O i -> O i -> O ($ i)
}

fun emb : Nat -> O #
{ emb zero = Z #
; emb (succ n) = S # (emb n)
}

let pre : [i : Size] -> (Nat -> O ($$ i)) -> Nat -> O ($ i)
  = \ i -> \ f -> \ n -> case (f (succ n))
    { (Z .($ i))   -> Z i
    ; (S .($ i) x) -> x
    ; (L .($ i) g) -> g n
    ; (M .($ i) a b) -> a
    }

{- the following pattern match is the problem: (L .($$ i) f)

   The correct pattern is (L .i f).  By subtyping these two terms are
   both of the required type O ($$$ i), but the least size must
   be chosen to be on the safe side.

   Thus, the implementation of dot patterns has to be changed to account
   for subtyping.

-}
trustme -- termination check fails (rightly so)
fun deep : [i : Size] -> O i -> Nat -> Nat
{ deep .($$$$ i) (M .($$$ i)
                    (L .($$ i) f)
                    (S .($$ i) (S .($ i) (S i x)))) n
  = deep ($$$ i) (M ($$ i) (L ($ i) (pre i f)) (f n))
      (succ (succ (succ n)))
--   = deep ($$$ i) (M ($$ i) (L ($ i) (pre i f)) (S ($ i) (pre i f n)))
--       (succ (succ (succ n)))
-- -- WHY DOES THIS TYPECHECK
--  = deep _ (M _ (L _ (pre _ f)) (S _ (f n))) (succ (succ (succ n)))
; deep i x n = n
}

let four : Nat
  = succ (succ (succ (succ zero)))

-- eval
let loop : Nat = deep # (M # (L # emb) (emb four)) four

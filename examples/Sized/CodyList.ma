{- 2010-02-02 Cody Roux communicated and observation of Frederic
   Blanqui that the "non-linear" size-assignment for constructors (see
   M below) does not allow to express the precise sizes in a deep
   match involving a limit ordinal (see L below).  From this I could
   construct a non-looping term in MiniAgda -}


data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data List (+ A : Set) : Set
{ nil : List A
; cons : A -> List A -> List A
}

sized data O : Size -> Set
{ Z : [i : Size] -> O ($ i)
; S : [i : Size] -> O i -> O ($ i)
; L : [i : Size] -> (Nat -> O i) -> O ($ i)
; N : [i : Size] -> List (O i) -> O ($ i)
}

fun emb : Nat -> O #
{ emb zero = Z #
; emb (succ n) = S # (emb n)
}

let pre : [i : Size] -> (Nat -> O ($ ($ i))) -> Nat -> O ($ i)
  = \ i -> \ f -> \ n -> case (f (succ n))
    { (Z .($ i))   -> Z i
    ; (S .($ i) x) -> x
    ; (L .($ i) g) -> g n
    ; (N .($ i) l) -> Z i
    }

{- the following pattern match is the problem: (L .($ ($ i)) f)

   The correct pattern is (L .i f).  By subtyping these two terms are
   both of the required type O ($ ($ ($ i))), but the least size must
   be chosen to be on the safe side.

   Thus, the implementation of dot patterns has to be changed to account
   for subtyping.

-}
trustme -- termination check fails (rightly so)
fun deep : [i : Size] -> O i -> Nat -> Nat
{ deep .($ ($ ($ ($ i))))
    (N .($$$ i)
       (cons {- .(O ($$$ i)) -} (L .($ ($ i)) f)
         (cons {- .(O ($$$ i)) -} (S .($ ($ i)) (S .($ i) (S i x)))
           (nil {-.(O ($$$ i)) -}))))
    n
  = deep _ (N _ (cons {-(O _)-} (L _ (pre _ f)) (cons {- (O _)-}  (S _ (f n)) (nil {-(O _)-}))))
           (succ (succ (succ n)))
; deep i x n = n
}

let four : Nat
  = succ (succ (succ (succ zero)))

-- eval
let loop : Nat = deep # (N # (cons (L # emb) (cons (emb four) nil))) four

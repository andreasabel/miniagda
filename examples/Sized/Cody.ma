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

{- 2010-03-08 construct a value of size 5 -}

fun f01 : [i : Size] -> Nat -> O ($$$ i)
{ f01 i zero = Z i
; f01 i (succ zero) = S _ (Z i)
; f01 i (succ (succ n)) = S _ (S _ (Z i))
}

let v5 : [i : Size] -> O ($$$$$ i)
  = \ i -> M _ (L _ (f01 i)) (S _ (S _ (S _ (Z i))))

fun emb : Nat -> O #
{ emb zero = Z #
; emb (succ n) = S # (emb n)
}

let pre : [i : Size] -> (Nat -> O ($ ($ i))) -> Nat -> O ($ i)
  = \ i -> \ f -> \ n -> case (f (succ n))
    { (Z .($ i))   -> Z i
    ; (S .($ i) x) -> x
    ; (L .($ i) g) -> g n
    ; (M .($ i) a b) -> a
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
{ deep .($ ($ ($ ($ i)))) (M .($ ($ ($ i))) 
                             (L .($ ($ i)) f) 
                             (S .($ ($ i)) (S .($ i) (S i x)))) 
                          n
  = deep _ (M _ (L _ (pre _ f)) (S _ (f n))) (succ (succ (succ n)))
; deep i x n = n   
}

let four : Nat 
  = succ (succ (succ (succ zero)))

--eval
let loop : Nat = deep # (M # (L # emb) (emb four)) four

{- 2010-03-08  Limits and deep matching

fun deep : [i : Size] -> O i -> Nat -> Nat
{ deep i4 (M i3 (L j2 f) (S i2 (S i1 (S i x)))) n
  = deep _ (M _ (L _ (pre _ f)) (S _ (f n))) (succ (succ (succ n)))
; deep i x n = n   
}

generates constraints
  i < i1 < i2 < i3 < i4  and  j2 < i3

id : [i : Size] -> List i -> List i
id i (cons j x xs) = cons j x (id j xs)

  $ j <= i
  cons j x (id j xs) : List ($ j)  <= List i

msort : [i : Size] -> List i -> List #
msort i2 (cons i1 x1 (cons i0 x0 xs)) =
  let (l,r) = split i0 xs in 
  merge (msort i1 (cons i0 x1 l)) 
        (msort i1 (cons i0 x0 r)) 

Explicit syntax:

msort : [i : Size] -> List i -> List #
msort i2 (cons (i2 > i1) x1 (cons (i1 > i0) x0 xs)) =
  let (l,r) = split i0 xs in 
  merge (msort i1 (cons i0 x1 l)) 
        (msort i1 (cons i0 x0 r))



merge : [i : Size] -> List i -> [j : Size] -> List j -> List #
merge i1 (cons i0 x xs))
      j1 (cons j0 y ys)) = 
   merge i1 (cons i0 x xs) j0 ys
OR merge i0 xs j1 (cons j0 y ys) 

-}

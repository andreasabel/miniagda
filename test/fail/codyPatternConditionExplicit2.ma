{- 2010-02-02 Cody Roux communicated and observation of Frederic
   Blanqui that the "non-linear" size-assignment for constructors (see
   M below) does not allow to express the precise sizes in a deep
   match involving a limit ordinal (see L below).  From this I could
   construct a non-looping term in MiniAgda.
 
   2010-03-09  
   This file tests whether the loop is still accepted after the fix.
 -}

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
  = \ i -> M $$$$i (L $$$i (f01 i)) (S $$$i (S $$i (S $i (Z i))))

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

fun deep : [i : Size] -> O i -> Nat -> Nat
{ deep i4 
   (M (i4 > i3) 
        (L (i3 > j2) f) 
        (S (i3 > i2)  
             (S (i2 > i1) 
                  (S (i1 > i) x)))) n                        -- illtyped! vv
  = deep (max ($$$ i) ($$ j2)) (M (max ($$ i) ($ j2)) (L ($ i) (pre ($$ i) f)) (S j2 (f n))) (succ (succ (succ n)))
      --   8          9        10        11       12
; deep i x n = n   
}


let four : Nat 
  = succ (succ (succ (succ zero)))

eval let loop : Nat = deep # (M # (L # emb) (emb four)) four

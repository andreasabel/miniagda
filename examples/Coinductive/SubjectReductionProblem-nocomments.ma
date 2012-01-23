{- 2010-05-28

   Thorsten, http://sneezy.cs.nott.ac.uk/fplunch/weblog/?p=104 
-}

sized codata Stream : Size -> Set 
{ cons : [i : Size] -> Stream i -> Stream ($ i)
}

cofun ticks : [i : Size] -> Stream i
{ ticks ($ i) = cons i (ticks i)
}

data Eq (A : Set)(a : A) : A -> Set
{ refl : Eq A a a
}

let l1 : Eq (Stream #) (ticks #) (cons # (ticks #)) 
       = refl -- (Stream #) (ticks #)

fun l2 :  (s : Stream #) -> (t : Stream #) -> (Eq (Stream #) s t)
-> Eq (Stream #) (cons # s) (cons # t)
{ l2 s .s (refl {- .(Stream #) .s -}) = refl -- (Stream #) (cons # s)
}

let l3 : Eq (Stream #) (cons # (ticks #)) (cons # (cons # (ticks #)))
       = l2 (ticks #) (cons # (ticks #)) l1

let l3' : Eq (Stream #) (cons # (ticks #)) (cons # (cons # (ticks #)))
        = refl -- (Stream #) (ticks #)


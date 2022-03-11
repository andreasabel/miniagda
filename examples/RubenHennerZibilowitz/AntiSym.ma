sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; suc  : [i : Size] -> Nat i -> Nat ($ i)
}

{-
data Eq : Nat # -> Nat # -> Set
{ refl : [i : Size] -> [n : Nat i] -> Eq n n
}
-}

data Eq : Nat # -> Nat # -> Set
{ refl : [n : Nat #] -> Eq n n
}

-- POINTLESS:
data Leq : Nat # -> Nat # -> Set
{ zLn : [i : Size] -> [j : Size] -> [n : Nat j] -> Leq (zero i) n
; sLs : [i : Size] -> [j : Size] -> [m : Nat i] -> [n : Nat j] ->
        Leq m n -> Leq (suc i m) (suc j n)
}

fail
fun antisym : [i : Size] -> [j : Size] -> [m : Nat i] -> [n : Nat j] ->
              Leq m n -> Leq n m -> Eq m n
{ antisym .($ i) .($ j) .(zero i) .(zero j)
    (zLn .i j n) (zLn .j i m) = refl (zero #)
{- TO DO FINISH
; antisym (sLs mLn) (sLs nLm) = case (antisym mLn nLm) of
   { refl -> refl }
-}
}

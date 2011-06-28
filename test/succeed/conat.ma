sized codata CoNat : Size -> Set
{ zero : [i : Size] -> CoNat ($ i) 
; succ : [i : Size] -> CoNat i -> CoNat ($ i)  
}

sized codata CoNatEq : (i : Size) -> CoNat i -> CoNat i -> Set
{ eqz : [i : Size] -> CoNatEq ($ i) (zero i) (zero i)
; eqs : [i : Size] -> (n : CoNat i) -> (m : CoNat i) -> 
   CoNatEq i n m -> CoNatEq ($ i) (succ i n) (succ i m)
}

cofun add : [i : Size] -> CoNat i -> CoNat i -> CoNat i
{ add ($ i) (zero .i)   n = n
; add ($ i) (succ .i m) n = succ i (add i m n)
}

cofun mult : [i : Size] -> CoNat i -> CoNat i -> CoNat i
{ mult ($ i) (zero .i)   n           = zero i
; mult ($ i) (succ .i m) (zero .i  ) = zero i
; mult ($ i) (succ .i m) (succ .i n) = succ i (add i n (mult i m (succ i n)))
}

{-
-- addmult n m = n*m + m
cofun addmult : [i : Size] -> CoNat # -> CoNat i -> CoNat i
{ addmult i (zero .#) n = n
; addmult i (succ .# m) n = add i n (addmult i m n)
}
-}

-- (n + 1)^(m + 1) = (n+1) * (n+1) ^ m = (n+1) ^ m + n * (n+1) ^ m
-- expinc m n = (n+1) ^ m
-- expinc 0 n = 1
-- expinc (m+1) n = (n+1) * expinc m n = addmult n (expinc m n)

-- cofun expinc : [i : Size] -> CoNat # -> CoNat i -> CoNat i

-- pexp m n = (n+1)^m - 1
-- pexp 0     n     = 0
-- pexp (m+1) 0     = 0 
-- pexp (m+1) 1     = 2^(m+1) - 1 -- ??? 
-- pexp (m+1) (n+2) = 1 + n + (n+2) * pexp m (n+2)
-- (n + 2)^(m + 1) = (n+2) * (n+2) ^ m = (n+2) ^ m + n * (n+1) ^ m
{-
cofun exp : [i : Size] -> CoNat i -> CoNat i -> CoNat i
{ exp ($ i) (zero .i  ) n           = succ i (zero i)
; exp ($ i) (succ .i m) (zero .i)   = zero i
; exp ($ i) (succ .i m) (succ .i n) = succ i (case i 
  { ($ j) -> case n of
    { (zero .j) ->
    ; (succ .j n) ->
    } 
   })
}

(zero .i)) = succ i (zero i)
; exp ($ i) (succ .i m) (succ .i (zero .i)) = succ i (zero i)

-}
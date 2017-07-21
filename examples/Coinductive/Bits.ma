data Eq (A : Set)(a : A) : A -> Set
{ refl : Eq A a a
}

sized codata Bits : Size -> Set
{ low  : [i : Size] -> Bits i -> Bits ($ i)
; high : [i : Size] -> Bits i -> Bits ($ i)
}

cofun lows : [i : Size] -> Bits i
{ lows ($ i) = low i (lows i)
}

-- Nicolas Oury's stuff
-- not an irrefutable match
fun eta : Bits # -> Bits #
{ eta (low  .# s) = low  # s
; eta (high .# s) = high # s
}

fun ext : (xs : Bits #) -> Eq (Bits #) xs (eta xs)
{ ext (low  .# xs) = refl -- (Bits #) (low  # xs)
; ext (high .# xs) = refl -- (Bits #) (high # xs)
}

let p : Eq (Bits #) (lows #) (low # (lows #))
      = ext (lows #)

let p2 : Eq (Bits #) (lows #) (low # (lows #))
       = refl -- (Bits #) (lows #)



-- Thorsten's stuff

let l1 : Eq (Bits #) (lows #) (low # (lows #))
       = refl -- (Bits #) (lows #)

fun l2 :  (s : Bits #) -> (t : Bits #) -> (Eq (Bits #) s t)
          -> Eq (Bits #) (low # s) (low # t)
{ l2 s .s (refl {-.(Bits #) .s-}) = refl -- (Bits #) (low # s)
}

let l3 : Eq (Bits #) (low # (lows #)) (low # (low # (lows #)))
       = l2 (lows #) (low # (lows #)) l1

let l3' : Eq (Bits #) (low # (lows #)) (low # (low # (lows #)))
        = refl -- (Bits #) (lows #)

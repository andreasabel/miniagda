sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; succ : [i : Size] -> Nat i -> Nat $i
}

fun minus : [i : Size] -> Nat i -> Nat # -> Nat i
{ minus i (zero (j < i))   n           = zero j
; minus i (succ (j < i) m) (zero .#)   = succ j m
; minus i (succ (j < i) m) (succ .# n) = minus j m n
}

-- Note 2013-03-30: not SN
cofun gcd : [i,j : Size] -> |i,j| -> Nat i -> Nat j -> Nat (max i j)
{ gcd i j (zero (i' < i))   n                 = n
; gcd i j (succ (i' < i) m) (zero (j' < j))   = succ i' m
; gcd i j (succ (i' < i) m) (succ (j' < j) n) =
    case minus i' m n
    { (zero (i'' < i'))      ->
      case minus j' n m
      { (zero (j'' < j'))    ->  -- in this case, m = n
          succ i' m
      ; (succ (j'' < j') n') ->  -- in this case n > m
          gcd i j' (succ i' m) (succ j'' n')
      }
    ; (succ (i'' < i') m')   ->  -- in this case, m > n
        gcd i' j (succ i'' m') (succ j' n)
    }
}

let one   : Nat # = succ # (zero #)
let two   : Nat # = succ # one
let three : Nat # = succ # two
let four  : Nat # = succ # three
let five  : Nat # = succ # four
let six   : Nat # = succ # five

eval let t2 : Nat # = gcd # # four six
eval let t1 : Nat # = gcd # # five three

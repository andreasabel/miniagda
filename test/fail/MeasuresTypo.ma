-- 2010-07-26 explicit measures

data Bool : Set
{ true : Bool
; false : Bool
}

data N : Set
{ zz : N
; ss : N -> N
}

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> Nat i -> Nat ($ i)
}

mutual {

  fun even  : [i : Size] -> |i,$0| -> Nat i -> Bool
  { even i n = even' i n
  }

  fun even' : [i : Size] -> |i,0|  -> Nat i -> Bool
  { even' i (zero (i > j))   = true
  ; even' i (succ (i > j) n) = odd' i n  -- typo here, should be j
  }

  fun odd'  : [i : Size] -> |i,0|  -> Nat i -> Bool
  { odd' i (zero (i > j))   = false
  ; odd' i (succ (i > j) n) = even j n
  }
}

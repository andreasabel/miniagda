-- 2010-08-28 mutual data types

mutual {

  data Even : Set
  { ev0 : Even
  ; evS : Odd -> Even
  }

  data Odd : Set
  { oddS : Even -> Odd
  }

}

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

mutual {

  fun evenToNat : Even -> Nat
  { evenToNat ev0 = zero
  ; evenToNat (evS o) = suc (oddToNat o)
  }

  fun oddToNat : Odd -> Nat
  { oddToNat (oddS e) = suc (evenToNat e)
  }
}

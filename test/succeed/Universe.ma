-- 2010-08-28

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

mutual {

  data U : Set 
  { nat : U
  ; pi  : (a : U) -> (El a -> U) -> U
  }

  fun El : U -> Set
  { El nat = Nat
  ; El (pi a f) = (x : El a) -> El (f x)
  }
}
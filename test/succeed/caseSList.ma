-- 2012-01-22 parameters gone from constructors

data Nat : Set
{ zero : Nat
; suc  : Nat -> Nat
}

data Bool : Set
{ true  : Bool
; false : Bool
}

fun leq : Nat -> Nat -> Bool
{ leq zero n = true
; leq (suc m) zero = false
; leq (suc m) (suc n) = leq m n
}

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

fun True : ^Bool -> Set
{ True b = Id Bool b true
}
let triv : True true
         = refl

fun False : Bool -> Set
{ False b = Id Bool b false
}
let triv' : False false
          = refl

fun leFalse : (n : Nat) -> (m : Nat) -> False (leq n m) -> True (leq m n)
{ leFalse  n       zero   p = triv
; leFalse (suc n) (suc m) p = leFalse n m p
; leFalse zero    (suc m) () -- IMPOSSIBLE
}

data SList : Nat -> Set
{ snil  : SList zero
; scons : (shead : Nat) ->      -- I can erase this at compile-time, but
                                -- it should be present at run-time ??
          (stailindex : Nat) -> -- this should be erased at run-time ??
          [True (leq stailindex shead)] ->
          (stail : SList stailindex) ->
          SList shead
}

fun maxN : Nat -> Nat -> Nat
{ maxN n m = case leq n m
  { true -> m
  ; false -> n
  }
}

fun maxLemma : (n : Nat) -> (m : Nat) -> (k : Nat) ->
              True (leq n k) -> True (leq m k) -> True (leq (maxN n m) k)
{ maxLemma n m k p q = case leq n m
  { true  -> q
  ; false -> p
  }
}

fun insert : (m : Nat) -> (n : Nat) -> SList n -> SList (maxN n m)
{ insert m .zero snil = scons m zero triv snil
; insert m n (scons .n k p l) = case leq n m
  { true  -> scons m n triv (scons n k p l)
  ; false -> scons n (maxN k m) (maxLemma k m n p (leFalse n m triv'))
                   (insert m k l)
  }
}

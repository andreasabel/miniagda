-- 2010-09-17 descending list with good erasure behavior

data Bool : Set
{ true  : Bool
; false : Bool 
}

data Nat : Set 
{ zero : Nat
; suc  : Nat -> Nat
}

fun leq : Nat -> Nat -> Bool
{ leq zero n = true
; leq (suc m) zero = false
; leq (suc m) (suc n) = leq m n
}

-- decidable propositions

data Id (A : Set) (a : A) : A -> Set
{ refl : Id A a a
}

fun True : Bool -> Set
{ True b = Id Bool b true
}
let triv : True true
         = refl Bool true

fun False : Bool -> Set
{ False b = Id Bool b false
}
let triv' : False false
          = refl Bool false

fun contradiction : (b : Bool) -> True b -> False b -> (A : Set) -> A
{ contradiction .true (refl .Bool .true) () A
}

-- properties of leq

let Leq : (n, m : Nat) -> Set
  = \ n m -> True (leq n m)

fun leFalse : (n : Nat) -> (m : Nat) -> False (leq n m) -> Leq m n
{ leFalse  n       zero   p = triv
; leFalse (suc n) (suc m) p = leFalse n m p
; leFalse zero    (suc m) () -- IMPOSSIBLE
}

-- leq is a partial order

fun reflexive : (n : Nat) -> Leq n n    -- < triv : Leq n n > -- does not type check
{ reflexive  zero   = triv
; reflexive (suc n) = reflexive n
} -- n is in fact irrelevant, but this is another analysis...

fun transitive : (k, l, m : Nat) -> [Leq k l] -> [Leq l m] -> Leq k m 
{ transitive (suc k)  zero    m      () q
; transitive  k      (suc l)  zero   p ()
; transitive  zero    zero    m      p q = triv
; transitive  zero   (suc l) (suc m) p q = transitive zero l m triv q
; transitive (suc k) (suc l) (suc m) p q = transitive k l m p q
}

-- sorted list (descendingly)

data SList *(bound : Nat) : Set  -- bound is the thing we will add next 
{ snil  : SList bound
; scons : (shead : Nat) ->   
          [True (leq shead bound)] -> 
          (stail : SList shead) -> 
          SList bound
} 

-- maximum and its properties

fun maxN : Nat -> Nat -> Nat
{ maxN n m = case leq n m 
  { true -> m
  ; false -> n
  }
}

fun maxBelow : (n : Nat) -> (m : Nat) -> (k : Nat) ->
              Leq n k -> Leq m k -> Leq (maxN n m) k
{ maxBelow n m k p q = case leq n m 
  { true  -> q
  ; false -> p
  } 
}

fun rLeqMax : (n, m : Nat) -> Leq m (maxN n m)
{ rLeqMax n m = case leq n m 
  { true  -> reflexive m
  ; false -> leFalse n m triv'
  }
}

{-
fun maxAbove : (k, m, n : Nat) -> False (leq k m) -> Leq k n -> Leq k (maxN n m)
{ maxAbove k m n p q = case leq n m 
  { true  -> contradiction (leq k m) (transitive k n m q triv) p (Leq k (maxN n m))
  ; false -> q
  }
}
-}

fun maxAbove : (k, m, n : Nat) -> Leq k n -> Leq k (maxN n m)
{ maxAbove k m n p = case leq n m 
  { true  -> transitive k n m p triv
  ; false -> p
  }
}

-- sorted insertion

-- insert m n l
--   m = new element
--   n = bound of list l
fun insert : (m : Nat) -> [n : Nat] -> SList n -> SList (maxN n m)
{ insert m n (snil .n) = scons (maxN n m) m (rLeqMax n m) (snil m)
; insert m n (scons .n k p l) = case leq k m 
  { true  -> scons (maxN n m) m (rLeqMax n m) (scons m k triv l)
  ; false -> -- leq k m == false, p : leq k n == true, need k <= maxN n m
             scons (maxN n m) k (maxAbove k m n p)
                   (insert m k l)
  }
}

-- termination check fails, need different reduction strategy with case
fail fun insert' : (m : Nat) -> [n : Nat] -> SList n -> SList (maxN n m)
{ insert' m n l = 
  let [nm     : Nat     ] = maxN    n m in
  let [mLeqnm : Leq m nm] = rLeqMax n m in
  case l 
  { (snil .n) -> scons nm m mLeqnm (snil m)
  ; (scons .n k p l) -> case leq k m 
    { true    -> scons nm m mLeqnm (scons m k triv l)
    ; false   -> -- leq k m == false, p : leq k n == true, need k <= maxN n m
                 scons nm k (maxAbove k m n p) (insert' m k l)
    }
  }
}

data Bool : Set 
{ true  : Bool
; false : Bool
}

fun not : Bool -> Bool
{ not true = false
; not false = true
}

fun id : Bool -> Bool
{ id true  = true
; id false = false
}

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

-- the following lemma is attempted very clumsily, proof cannot be completed
-- with out another case distinction
fail
fun lemma : (x : Bool) -> Id Bool (not (id x)) (id (not x))
{ lemma x = case (not x)
  { true -> case (id x)
    { false -> refl Bool true
    -- ; true -> -- INCONSISTENT ASSUMPTIONS, however, not accessible to user
    }
  ; false -> case (id x)
    { true -> refl Bool false
    -- ; false -> -- INCONSISTENT
    }
  }
}

-- Lemma: f (f (f x)) = f x  for all f : Bool -> Bool
-- Proof by cases on x, f true, and f false
trustme -- 2010-11-01 this no longer works, since strong rule for if has been 
        -- disabled after discussions following AIM 12
fun tripleF : (f : Bool -> Bool) -> (x : Bool) -> Id Bool (f (f (f x))) (f x)
{ tripleF f true = case f true
  { true -> refl Bool true
  ; false -> case f false
    { true  -> refl Bool false
    ; false -> refl Bool false
    }
  }
; tripleF f false = case f false
  { true -> case f true 
    { true -> refl Bool true
    ; false -> refl Bool true
    }
  ; false -> refl Bool false
  }
}
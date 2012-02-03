-- 2012-01-26 infer type of pair

data Bool : Set { true; false }

{- 2012-02-03 pair inference disabled because of irrelevance
   would need polarity annotation in first component in general

let xor (a, b : Bool) : Bool
  = case a, b   -- infers type of (a,b)
    { (true, true) -> false
    ; (false, true) -> true
    ; (true, false) -> true
    ; (false, false) -> false
    }
-}

let xor' (a, b : Bool) : Bool
  = case (a,b) : Bool & Bool
    { (true, true) -> false
    ; (false, true) -> true
    ; (true, false) -> true
    ; (false, false) -> false
    }

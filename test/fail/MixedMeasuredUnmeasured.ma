-- 2010-07-17

-- mixing measured functions with unmeasured is illegal,
-- caught by the scope-checker

mutual {
  fun f : (i : Size) -> |i| -> Set {}
  fun g : (i : Size) -> Set {}
}


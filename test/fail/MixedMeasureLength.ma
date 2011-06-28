-- 2010-07-17 

-- measured functions need to have the same length measure
-- caught by the scope-checker

mutual {
  fun f : (i,j : Size) -> |i| -> Set {}
  fun g : (i,j : Size) -> |i,j| -> Set {}
}


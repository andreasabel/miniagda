-- 2010-09-22

-- a buggy PTS rule might check whether the sort of the domain
-- is leq than the sort of the codomain

let T : (i : Size) -> Set ($$ i)
  = \ i -> Set ($ i) -> Set i

let U : (i : Size) -> Set _
  = \ i -> Set ($ _) -> Set _


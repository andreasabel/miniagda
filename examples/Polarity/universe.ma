-- 2010-08-28

data Empty : Set {}

mutual {

  sized data U : Size -> Set 
  { u  : [i : Size] -> U $i
  ; pi : [i : Size] -> (a : U i) -> (f : T i a -> U i) -> U $i
  }

  -- TODO: fun T : [i : Size] -> |i| -> U i -> Set
  fun T : [i : Size] -> U i -> Set
  { T i (u  (i > j)    ) = Empty 
  ; T i (pi (i > j) a f) = (x : T j a) -> T j (f a)
  }
}
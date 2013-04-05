-- 2012-02-05  Check whether we can define dependent case in MiniAgda

let Map (F : Set -> Set)
  = [A, B : Set] -> (A -> B) -> F A -> F B

cofun Nu : +(F : +Set -> Set) -(i : Size) -> Set
{ Nu F i = F ([j < i] -> Nu F j)
}

{- IMPOSSIBLE?
let inst [F : +Set -> Set] (map : Map F) [i : Size] (f : [j < $i] -> Nu F j)
  : Nu F i
  = \ j -> map () () (f $j) : Nu F $j -- ??
-}

-- destructor

let out [F : +Set -> Set] (map : Map F) [i : Size] : Nu F $i -> F (Nu F i)
  = map ([j <= i] -> Nu F j) (Nu F i) (\ r -> r i)

-- constructor

let inn [F : +Set -> Set] (map : Map F) [i : Size] : F (Nu F i) -> Nu F $i
  = map (Nu F i) ([j <= i] -> Nu F j) (\ t j -> t)

-- coiteration

cofun coit : [F : +Set -> Set] (map : Map F)
  [S : Set] (step : S -> F S) (start : S)
  [i : Size] -> |i| -> Nu F i
{ coit F map S step start i
    = map S ([j < i] -> Nu F j) -- (\ s j -> coit F map S step s j)
       (coit F map S step)
       (step start)
}

-- eta

let eta [F : +Set -> Set] (map : Map F) [i : Size] (r : Nu F $i) : Nu F $i
  = inn F map i (out F map i r)

fun caseNu : [F : +Set -> Set] (map : Map F)
  [P : (i : Size) -> Nu F i -> Set]
  (f : [i : Size] -> (t : F (Nu F i)) -> P $i (inn F map i t))
  [i : Size] (x : Nu F $i) -> P $i (eta F map i x)
{ caseNu F map P f i x = f i (out F map i x)
}


-- case
-- fails, because eta does not hold for Nu in this encoding (map is in the way)

fail
let caseO
  [F : +Set -> Set] (map : Map F)
  [P : (i : Size) -> Nu F i -> Set]
  (f : [i : Size] -> (t : F (Nu F i)) -> P $i (inn F map i t))
  [i : Size]
  (x : Nu F $i) : P $i x
                = f i (out F map i x)

-- subject reduction test

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

-- caseNu F (\ i x -> Id () x (inn F i (F (coit F map S f

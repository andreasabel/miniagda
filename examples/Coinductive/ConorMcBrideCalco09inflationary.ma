-- 2012-02-05  Check whether we can define dependent case in MiniAgda

let Map (F : Set -> Set)
  = [A, B : Set] -> (A -> B) -> F A -> F B

cofun Nu : (F : Set -> Set) -(i : Size) -> Set
{ Nu F i = [j < i] -> F (Nu F j)
}

-- constructor (needs monotonicity)

check
fun inn : [F : +Set -> Set] [i : Size] -> F (Nu F i) -> Nu F $i
{ inn F i t j = t
}

-- destructor

let outf [F : Set -> Set] [i < #] (r :  Nu F $i) : F (Nu F i)
  = r i
-- SHOULD WE DROP $# = # ?  Then outf would hold also for any i : Size

let outif [F : Set -> Set] [i < #] (r :  Nu F #) : F (Nu F i)
  = r i

{-
let out [F : +Set -> Set] (map : Map F) [i : Size] (r :  Nu F $i) : F (Nu F i)
  = map () () r i
-}

-- constructor

let inn [F : +Set -> Set] [i : Size] (t : F (Nu F i)) : Nu F $i
  = \ j -> t

-- coiteration

cofun coit : [F : +Set -> Set] (map : Map F)
  [S : Set] (step : S -> F S)
  [i : Size] -> |i| -> (start : S) -> Nu F i
{ coit F map S step i start
    = \ j -> map S (Nu F j) (coit F map S step j) (step start)
}

{- not needed (eta is built-in)
-- eta

let eta [F : +Set -> Set] [i : Size] (r : Nu F $i) : Nu F $i
  = \ j -> r j

fun caseNu : [F : +Set -> Set]
  [P : (i : Size) -> Nu F i -> Set]
  (f : [i : Size] -> (t : F (Nu F i)) -> P $i (inn F i t))
  [i : Size] (x : Nu F $i) -> P $i (eta F i x)
{ caseNu F P f i x = f i (x i)
}
-}

-- case

let caseNu
  [F : +Set -> Set]
  [P : (i : Size) -> Nu F i -> Set]
  (f : [i : Size] -> (t : F (Nu F i)) -> P $i (inn F i t))
  [i : Size]
  (x : Nu F $i) : P $i x
                = f i (x i)

-- subject reduction test

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

-- caseNu F (\ i x -> Id () x (inn F i (F (coit F map S f

-- 2012-02-06  Make sure not to violate < - Constraints by going through infty
-- (not finished)

fun fix : [F : Size -> Set]
          (phi : [i <= #] (f : [j < i] -> F j) -> F i)
          [i <= #] -> |i| -> F i
{ fix F phi i = phi i (fix F phi)
}

cofun Bot : +(i : Size) -> Set
{ Bot i = [j < i] & Bot j
}

cofun Top : -(i : Size) -> Set
{ Top i = [j < i] -> Top j
}

let out [i : Size] (r :  Top $i) : Top i
  = \ j -> r $j j

let inn [i : Size] (t : Top i) : Top $i
  = \ j -> t

let bad [F : Size -> Set] [i <= #] (f : [j < $i] -> F j) : F i
  = f i

let test [F : Size -> Set] = fix F (bad F)

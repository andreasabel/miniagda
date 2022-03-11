-- 2010-11-06

sized codata Stream ++ (A : Set) : -Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A $i
} fields head, tail

cofun iterate
  : [A : Set ] -> (step : A -> A) -> (start : A) ->
    [i : Size] -> Stream A i
{ iterate A step start ($ i) = cons i start (iterate A step (step start) i)
}

-- this might be accepted without trustme in future versions?!
trustme
data Tm : Set
{ abs : (^Tm -> Tm) -> Tm
; app : Tm -> Tm -> Tm
}

fun sapp : ^Tm -> Tm
{ sapp x = app x x
}

let delta : Tm
  = abs (\ x -> sapp x)

let omega : Tm
  = app delta delta

fun step : Tm -> Tm
{ step (app (abs f) t) = f t
; step (app t u) = app (step t) u
; step (abs f)   = abs f
}

let steps : Tm -> Stream Tm #
  = \ start -> iterate Tm step start #

-- eval
let omegas : Stream Tm # = steps omega


data Ty : Set
{ base : Ty
; arr  : Ty -> Ty -> Ty
}

fun eta : Ty -> ^Tm -> Tm
{ eta base t = t
; eta (arr a b) t = abs (\ x -> eta b (app t (eta a x)))
}

let tid : Tm = abs (\x -> x)
eval let tid2 : Tm = eta (arr (arr base base) (arr base base)) tid

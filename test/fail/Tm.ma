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
{ abs : ^(^Tm -> Tm) -> Tm
; app : ^Tm -> ^Tm -> Tm
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
; step (abs f)   = abs (\ x -> step (f x))
  -- rejected, since x not parametric
  -- think of f=id, then step would analyze x !
}

let steps : Tm -> Stream Tm #
  = \ start -> iterate Tm step start #

eval let omegas : Stream Tm # = steps omega

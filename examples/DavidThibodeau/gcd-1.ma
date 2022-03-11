-- 2011-11-25

sized data SNat : Size -> Set
{ zero : (i : Size) -> SNat ($ i)
; succ : (i : Size) -> SNat i -> SNat ($ i)
}

fun minus : [i : Size] -> SNat i -> SNat # -> SNat i
{   minus i (zero (i > j))     y                 = zero j
;   minus i x                  (zero .#)         = x
;   minus i (succ (i > j) x)   (succ .# y)       = minus j x y
}

data LessSize : [i : Size] -> [j : Size] -> (a : SNat i) -> (b : SNat j) -> Set
{ baseSize : [i : Size] -> [j : Size] -> [a : SNat j] ->
   LessSize ($i) ($j) (zero i) (succ j a)
; stepSize : [i : Size] -> [j : Size] -> [a : SNat i] -> [b : SNat j] ->
   LessSize i j a b -> LessSize $i $j (succ i a) (succ j b)
}

data Maybe (A : Set) : Set
{ nothing : Maybe A
; just : A -> Maybe A
}

fun lessThanSize : [i : Size] -> [j : Size] -> (a : SNat i) ->
                   (b : SNat j) -> Maybe (LessSize i j a b)
{ lessThanSize i j (zero (i > k)) (zero (j > l)) =
               nothing -- (LessSize i j (zero k) (zero l))
; lessThanSize i j (zero (i > k)) (succ (j > l) b) =
               just {- (LessSize i j (zero k) (succ l b)) -} (baseSize i j b)
; lessThanSize i j (succ (i > k) a) (zero (j > l)) =
               nothing -- (LessSize i j (succ k a) (zero l))
; lessThanSize i j (succ (i > k) a) (succ (j > l) b) =
               case lessThanSize i j a b
               { (nothing {- .(LessSize i j a b) -}) ->
                 nothing -- (LessSize $i $j (succ i a) (succ j b))
               ; (just {- .(LessSize i j a b) -} m) ->
                 just {- (LessSize $i $j (succ i a) (succ j b)) -}
                   (stepSize i j a b m)
               }
}

-- does not termination check
fail
fun gcd : [i : Size] -> [j : Size] -> SNat i -> SNat j -> SNat (max i j)
{ gcd i j (zero (i > k))   y                = y
; gcd i j (succ (i > k) x) (zero (j > l))   = succ k x
; gcd i j (succ (i > k) x) (succ (j > l) y) =
      case lessThanSize i j (succ k x) (succ l y)
      { (just {- .(LessSize i j (succ k x) (succ l y)) -} m) ->
          gcd $l $k (succ l y) (succ k x)
      ; (nothing {- .(LessSize i j (succ k x) (succ l y)) -}) ->
          gcd k $l (minus k x y) (succ l y)
      }
}

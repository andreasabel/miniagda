-- 2012-01-31  Examples for FICS 2012 talk

-- Compositional termination

data List (A : Set) { nil ; cons (a : A) (as : List A) }

check
fun everyOther : [A : Set] -> List A -> List A
{ everyOther A nil                   = nil
; everyOther A (cons a nil)          = nil
; everyOther A (cons a (cons a' as)) = cons a (everyOther A as)
}

fun zeroOneMany : [A : Set] -> List A -> [C : Set] -> 
  C ->                       -- zero
  (A -> C) ->                -- one 
  (A -> A -> List A -> C) -> -- many
  C
{ zeroOneMany A nil                   C zero one many = zero
; zeroOneMany A (cons a nil)          C zero one many = one a
; zeroOneMany A (cons a (cons a' as)) C zero one many = many a a' as
}

fail -- termination check fails
fun everyOther : [A : Set] -> List A -> List A
{ everyOther A l = zeroOneMany A l (List A) 
    nil
    (\ a       -> nil)
    (\ a a' as -> cons a (everyOther A as)) 
}
-- 2012-01-31  Examples for FICS 2012 talk

-- Compositional termination with sized types

data List ++(A : Set) +(i : Size) : Set
{ nil                                  : List A i
; cons [j < i] (a : A) (as : List A j) : List A i
}

fun everyOther : [A : Set] -> [i : Size] -> |i| -> List A i -> List A i
{ everyOther A i nil                       = nil
; everyOther A i (cons j a nil)            = nil
; everyOther A i (cons j a (cons k a' as)) = cons j a (everyOther A j as)
}

fun zeroOneMany : [A : Set] -> [i : Size] -> List A i -> [C : Set] ->
  (zero : C) ->
  (one  : A -> C) ->
  (many : [j < i] -> A -> A -> List A j -> C) ->
  C
{ zeroOneMany A i nil                       C zero one many = zero
; zeroOneMany A i (cons j a nil)            C zero one many = one a
; zeroOneMany A i (cons j a (cons k a' as)) C zero one many = many j a a' as
}

-- 2013-03-30 not SN
cofun everyOther1 : [A : Set] -> [i : Size] -> |i| -> List A i -> List A i
{ everyOther1 A i l = zeroOneMany A i l (List A i)
    nil
    (\ a -> nil)
    (\ j a a' as -> cons j a (everyOther1 A j as))
}

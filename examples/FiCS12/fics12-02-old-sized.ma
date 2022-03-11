-- 2012-01-31  Examples for FICS 2012 talk

-- Compositional termination with sized types

sized data List (A : Set) : Size -> Set
{ nil  [i : Size]                         : List A $i
; cons [i : Size] (a : A) (as : List A i) : List A $i
}

fun everyOther : [A : Set] -> [i : Size] -> List A i -> List A i
{ everyOther A .$i  (nil i)                     = nil i
; everyOther A .$$i (cons .$i a (nil i))        = nil i
; everyOther A .$$i (cons .$i a (cons i a' as)) = cons i a (everyOther A i as)
}

fun zeroOneMany : [A : Set] -> [i : Size] -> List A $i -> [C : Set] ->
  C ->                       -- zero
  (A -> C) ->                -- one
  (A -> A -> List A i -> C) -> -- many
  C
{ zeroOneMany A .$i  (nil i)                     C zero one many = zero
; zeroOneMany A .$$i (cons .$i a (nil i))        C zero one many = one a
; zeroOneMany A .$$i (cons .$i a (cons i a' as)) C zero one many = many a a' as
}

fail -- successor pattern not allowed
fun everyOther1 : [A : Set] -> [i : Size] -> List A $i -> List A $i
{ everyOther1 A $i l = zeroOneMany A i l (List A $i)
    (nil i)
    (\ a       -> nil i)
    (\ a a' as -> cons i a (everyOther1 A i as))
}

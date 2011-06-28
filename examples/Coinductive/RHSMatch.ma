sized codata St : Size -> Set
{ guard : [i : Size] -> St i -> St ($ i)
}
cofun st : [i : Size] -> St i
{ st ($ i) = guard i (st i)
}
{-

sized codata St (i : Size) -> Set
{ guard : exists j < i. St j
; guard : exists $ j = i. St j
}


cofun st : [i : Size] -> St i
{ st i = let (j < i) in guard j (st j)  -- guard (j < i) (st j)
}

-}
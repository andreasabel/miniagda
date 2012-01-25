-- 2012-01-25 parsing telescopes in constructor declarations

data List ++(A : Set) ++(i : Size) : Set
{ nil  [j < i]                         : List A i
; cons [j < i] (x : A) (xs : List A j) : List A i
}

sized data SList ++(A : Set) : +Size -> Set
{ snil  [i : Size]                          : SList A $i
; scons [i : Size] (x : A) (xs : SList A i) : SList A $i
}

{-
sized data IList ++(A : Set) : +Size -> Set
{ inil  [i <= #]                          : IList A $i
; icons [i <= #] (x : A) (xs : IList A i) : IList A $i
}
-}


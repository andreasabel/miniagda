-- 2010-11-26

data Bool : Set
{ true : Bool
; false : Bool
}

let C : Size -> Set
      = \ i -> [j : Size] -> |j| < |i| -> Bool

cofun foo : [i : Size] -> C i
{ foo ($i) j = true
}

{- does not type check
cofun loop : [i : Size] -> C i
{ loop ($i) j = loop i j
}
-}

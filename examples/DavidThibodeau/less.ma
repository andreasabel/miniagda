{- 2011-11-24

I have been trying to create a datatype to encode the lessthan relation. 
I succeeded in an attempt without size annotation in the following way

data less : [a : Nat] -> [b : Nat] -> Set
{ base : [a : Nat] -> less z a
; step : [a : Nat] -> [b: Nat] -> less a b -> less (s a) (s b)
}

However my attempts to add sizes have been unsuccessful. I tried the following
data lessSize : [i : Size] -> [j : Size] -> [a : SNat i] -> [b : SNat j] -> Set
{ baseSize : [i : Size] -> [a : SNat i] -> lessSize (zero i) a
; stepSize : [i : Size] -> [j : Size] -> [a : SNat i] -> [b: SNat j] -> lessSize a b -> 
                  lessSize (succ i a) (succ j b)
}
Which gives me errors for either of the constructors. I tried putting 
"sized" in front of "data" since a sized argument must be used implicitly 
for SNat but it requires me to set the polarity to +. I did not find any 
way to add + to the types that would not result in a parsing error.
Is there a way to make this work with sizes?

Thanks again
David
-}

sized data SNat : Size -> Set 
{ zero : [i : Size] -> SNat ($ i)
; succ : [i : Size] -> SNat i -> SNat ($ i)
}

sized data LessSize : (i : Size) -> (a : SNat i) -> (b : SNat i) -> Set
{ baseSize : [i : Size] -> [a : SNat $i] -> 
    LessSize ($i) (zero i) a
; stepSize : [i : Size] -> [a : SNat i] -> [b: SNat i] -> 
    LessSize i a b -> LessSize $i (succ i a) (succ i b)
}

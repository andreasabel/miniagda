-- 2010-05-19

data Bool : Set 
{ true  : Bool
; false : Bool
}

data Prod (+ A : Set) (+ B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B
}
fields fst, snd

sized codata BStr : Size -> Set
{ cons : [i : Size] -> (head : Bool) -> (tail : BStr i) -> BStr ($ i) 
}
fields head, tail

-- this code needs to be rejected by the type checker! :
-- a "function" returning the input stream plus its "last" bit
cofun idAndLast : [i : Size] -> BStr i -> Prod Bool (BStr i)
{ idAndLast ($ i) (cons .i b bs) = pair Bool (BStr ($ i))
   (fst Bool (BStr i) (idAndLast i bs))
   (cons i b          (idAndLast i bs))
}

cofun trues : [i : Size] -> BStr i
{ trues ($ i) = cons i true (trues i)
}

-- this will loop:
eval let last : Bool = snd Bool (BStr #) (idAndLast # (trues #))


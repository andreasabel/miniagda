-- 2010-05-19

data Unit : Set
{ unit : Unit
}

data Bool : Set 
{ true  : Bool
; false : Bool
}

data Twice (+ A : Set) : Set
{ inl : A -> Twice A
; inr : A -> Twice A
}

fun fmap : [A : Set] -> [B : Set] -> (A -> B) -> Twice A -> Twice B
{ fmap A B f (inl a) = inl (f a)
; fmap A B f (inr a) = inr (f a)
}

sized codata BStr : Size -> Set
{ cons : [i : Size] -> (head : Bool) -> (tail : BStr i) -> BStr ($ i) 
}

-- this code needs to be rejected by the type checker! :
-- a "function" returning the input stream plus its "last" bit
cofun idAndLast : [i : Size] -> BStr i -> Twice (BStr i) 
{ idAndLast ($ i) (cons .i b bs) = fmap (BStr i) (BStr ($ i))
   (cons i b) (idAndLast i bs)
}

cofun trues : [i : Size] -> BStr i
{ trues ($ i) = cons i true (trues i)
}

-- this will loop:
eval let last : Twice Unit = 
  fmap (BStr #) Unit (\ x -> unit) (idAndLast # (trues #))


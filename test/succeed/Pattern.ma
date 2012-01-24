-- 2012-01-23 pattern declarations

data Unit : Set { unit  : Unit }

-- * Booleans

data Bool : Set 
{ true  : Bool
; false : Bool
}

fun if : [i : Size] -> (A : Set i) -> Bool -> ++(a, b : A) -> A
{ if i A true  a b = a
; if i A false a b = b
}

fun If : Bool -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

-- * disjoint sum

let Plus : ++(A, B : Set) -> Set
  = \ A B -> (b : Bool) & If b A B

pattern inl a = true  , a
pattern inr b = false , b

fun casePlus : [A, B, C : Set] -> (A -> C) -> (B -> C) -> Plus A B -> C
{ casePlus A B C f g (inl a) = f a
; casePlus A B C f g (inr b) = g b
}

-- * Maybe

let Maybe : ++(A : Set) -> Set
  = Plus Unit

pattern nothing = inl unit
pattern just a  = inr a

fun maybe : [A, B : Set] -> B -> (A -> B) -> Maybe A -> B
{ maybe A B b f nothing  = b
; maybe A B b f (just a) = f a
}

let mapMaybe : [A, B : Set] -> (A -> B) -> Maybe A -> Maybe B
  = \ A B f -> maybe A (Maybe B) nothing (\ a -> just (f a))

-- * Lists

let ListF : ++(A, X : Set) -> Set
  = \ A X -> Maybe (A & X)

cofun List : ++(A : Set) -> ++(i : Size) -> Set
{ List A i = (j < i) & ListF A (List A j)
}

pattern nil  j      = j , nothing
pattern cons j a as = j , just (a , as)



{-
data Bit  : Set { b0 : Bit; b1 : Bit }

fun BitCase : Bit -> ++(A, B : Set) -> Set
{ BitCase b0 A B = A
; BitCase b1 A B = B
}
-}
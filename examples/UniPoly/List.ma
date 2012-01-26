

data Bool : Set { true : Bool; false : Bool }

data List ++(i : Size) ++(A : Set i) : Set i
{ nil  : List i A
; cons : A -> List i A -> List i A
}

data Eq (i : Size) (A : Set i)(a : A) : A -> Set (i + 1)
{ refl : Eq i A a a }

let empty0 : List 0 Bool = nil -- 0 Bool

let empty1 : List 1 Bool = nil -- 1 Bool

let bla : Eq 1 (List 1 Bool) empty0 empty1
  = refl -- 1 (List 1 Bool) empty0

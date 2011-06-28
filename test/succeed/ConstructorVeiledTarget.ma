-- 2010-09-14

let Id : ++ Set -> Set
 = \ A -> A

data Bool : Set
{ true : Bool
; false : Id Bool
} 
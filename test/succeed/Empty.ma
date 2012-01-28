-- 2012-01-28 the empty type as least type

data Empty {}

let abort [A : Set] (x : Empty) : A = x

let abort1 [A : Set] (x : Empty) : A -> A = x
let abort2 [F : +Set -> Set] [A : Set] (x : F Empty) : F A = x

let toEmp [A, B : Set] (x : A -> B) : Empty -> B = x

data Unit { unit }

let abort3 (x : Empty) : Unit = x
let abort4 (x : Empty) : |0| < |0| -> Unit = x
let abort5 (x : Empty) : [i < 0] -> Unit = x

-- unit type as the biggest type

data Bool { true; false }

fun f : Bool -> Unit
{ f x = x
}

let noReturnNeeded [M : +Set -> Set] [A : Set] (x : M A) : M Unit
  = x

fun g : Unit -> Bool
{ g unit = true -- this should translate into a variable pattern
}

let test [T : Bool -> Set] (x : T (g true)) : T true 
  = x
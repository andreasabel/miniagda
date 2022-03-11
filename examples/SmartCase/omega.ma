-- found with date 2009-12-16
-- a (failed) attempt to break normalization with smart case

data Unit : Set
{ unit : Unit
}

data Bool : Set
{ true  : Bool
; false : Bool
}

fun T : Bool -> Set
{ T true = Unit
; T false = Unit -> Unit
}

let id : (A : Set) -> A -> A
       = \ A -> \ a -> a

let app : (A : Set) -> (B : Set) -> (A -> B) -> A -> B
        = \ A -> \ B -> \ f -> \ x -> f x

let bla : Bool -> Unit
  = \ b -> case b
           { true -> case b
             { true -> unit
             ; false -> id (T true) unit
             }
           ; false -> unit
           }


-- 2012-03-06
-- more complicated case of comparing case clauses

data Id ++(A : Set) (x : A) : A -> Set
{ refl : Id A x x
}

data Unit : Set
{ unit : Unit
}

data Either ++(A, B : Set) : Set
{ left  : A -> Either A B
; right : B -> Either A B
}

let Maybe ++(A : Set) : Set =
  Either Unit A

pattern nothing = left unit
pattern just x  = right x

data Monad (F : +Set -> Set) : Set $0
{ monad :
    (return        : (A : Set) -> A -> F A) ->
    (bind          : (A, B : Set) -> F A -> (A -> F B) -> F B) ->
    (leftIdentity  : (A, B : Set) (x : A) (f : A -> F B) ->
                     Id (F B) (bind A B (return A x) f) (f x)) ->
    Monad F
}
fields return, bind, leftIdentity

let maybeT (F : +Set -> Set) (M : Monad F) : Monad (\A -> F (Maybe A))
  = monad (\A x -> return M (Maybe A) (just x))
          (\A B m f -> bind M (Maybe A) (Maybe B) m (\x -> case x
                         { nothing  -> return M (Maybe B) nothing
                         ; (just x) -> f x
                         }))
          (\A B x f -> leftIdentity M (Maybe A) (Maybe B) (just x)
                         (\x -> case x
                            { nothing  -> return M (Maybe B) nothing
                            ; (just x) -> f x
                            }))

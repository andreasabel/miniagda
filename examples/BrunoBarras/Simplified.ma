-- 2010-09-17

data True  : Set { trivial : True}
data False : Set {}

-- bad data declaration, makes all types convertible
trustme data Wrap [A : Set] : Set
{ wrap : (unwrap : A) -> Wrap A
}
fields unwrap

let boo : False
  = unwrap False (wrap True trivial)

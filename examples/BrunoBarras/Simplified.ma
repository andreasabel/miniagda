-- 2010-09-17

data True  : Set { trivial : True}
data False : Set {}

-- bad data declaration, makes all types convertible
trustme data Wrap [A : Set] : Set
{ wrap : (unwrap : A) -> Wrap A
}
fields unwrap

let wrap_ : [A : Set] -> A -> Wrap A
  = \ A a -> wrap a

let foo : Wrap False = wrap_ True trivial

let boo : False
  = unwrap {- False -} foo -- (wrap_ True trivial)


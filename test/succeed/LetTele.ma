-- 2012-01-24 telescopes for let

let id0 : [i : Size] -> [A : Set i] -> (a : A) -> A 
  = \ i A a -> a

let id [i : Size][A : Set i](a : A) : A = a

-- 2012-01-26 let and local let without type

let id' [i : Size][A : Set i](a : A) = a

let two [A : Set] (f : A -> A) (x : A) : A =
  let y = f x
  in  f y 

let two' : [A : Set] -> (f : A -> A) -> (x : A) -> A =
  \ A f x ->
  let y = f x
  in  f y 
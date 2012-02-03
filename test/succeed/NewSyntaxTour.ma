-- 2012-01-27

-- Telescopes in let-declarations
----------------------------------------------------------------------

-- instead of

let two : [A : Set] -> (f : A -> A) -> (a : A) -> A
  = \ A f a -> f (f a)

-- one can now write

let two1 [A : Set] (f : A -> A) (a : A) : A
  = f (f a)

-- since the type A of the let-body f (f a) is inferable
-- we can omit it

let two2 [A : Set] (f : A -> A) (a : A)
  = f (f a)

-- telescopes can also contain bounded size variables

let boundedSize (j <= #) (i < j) = i

-- Untyped local let
----------------------------------------------------------------------

-- inferable types of local let declarations can also be omitted

let twice [F : Set -> Set] (f : [A : Set] -> A -> F A)
          [A : Set] (a : A) : F (F A)
  = let [FA] = F A   in
    let fa   = f A a in f FA fa

-- local lets can also use telescopes
let localLetTel : Size =
  let two1 [A : Set] (f : A -> A) (a : A)
    = f (f a)
  in 0

-- and can still be made irrelevant
let localLetIrr [A : Set] (f : [A -> A] -> Size) [a : A] : Size =
  let [g] (x : A) = a
  in  f g

-- alternative with . instead of brackets
let localLetIrr1 [A : Set] (f : .(A -> A) -> Size) .(a : A) : Size =
  let .g (x : A) = a
  in  f g

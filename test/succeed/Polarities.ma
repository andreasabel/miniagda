-- 2010-06-19, 2010-11-09

let Const : ++ Set -> . Set -> Set 
          = \ A -> \ X -> A

let DNeg : ^ Set -> + Set -> Set
         = \ B -> \ A -> * (* A -> B) -> B

data Empty : Set {}

sized data Nat : + Size -> Set
{ zero : [i : Size] -> Nat ($ i)
; succ : [i : Size] -> ^ Nat i -> Nat ($ i)
}

let Cont' : + Set -> Set
         = DNeg Empty

-- the following holds already because of whnf computation
let cast' : [i : Size] -> ^ Cont' (Nat i) -> Cont' (Nat #)
         = \ i -> \ x -> x

data Cont +(A : Set) : Set 
{ cont : (uncont : DNeg Empty A) -> Cont A
}

-- the following holds because Cont is a datatype (pol. already impl.)
let cast : [i : Size] -> ^ Cont (Nat i) -> Cont (Nat #)
         = \ i -> \ x -> x

-- hide positivity behind recursion
fun Id : * Nat # -> ++Set -> Set
{ Id (zero .#)   A = A
; Id (succ .# n) A = A
}

let kast : [i : Size] -> [n : Nat i] -> Id n (Nat i) -> Id n (Nat #)
         = \ i -> \ n -> \ x -> x

data Tree -(B : Set) ++(A : Set) : Set
{ leaf : Tree B A
; node : A -> (B -> Tree B A) -> Tree B A
}

sized data STree -(B : Set) ++(A : Set) : +Size -> Set
{ sleaf : [i : Size] -> STree B A ($ i)
; snode : [i : Size] -> A -> (B -> STree B A i) -> STree B A ($ i)
}

data Mu ++(F : ++Set -> Set) : Set
{ inn : F (Mu F) -> Mu F
}

{-
  .(p)  = o
  ++(p) = p
  +(++) = +
  +(p)  = p
  -(++) = -
  -(+)  = -
  -(-)  = +
  -(p)  = p 
  o(o)  = o
  o(++) = .
  o(+)  = .
  o(-)  = .
  o(.)  = .

  -(Gamma) |- A : s  Gamma |- B : s
  ---------------------------------
  Gamma |- A -> B : s

  -(Gamma) |- A : s  Gamma, x : A |- B : s
  ----------------------------------------
  Gamma |- p(x : A) -> B : s

  --------------------------------  p in {++,+,o}
  Gamma, p(x : A), Gamma' |- x : A
  
  Gamma, p(x : A) |- t : B
  ----------------------------
  Gamma |- \xt : p(x : A) -> B

  Gamma |- r : p(x : A) -> B   p(Gamma) |- s : A
  ----------------------------------------------
  Gamma |- r s: B[s/x]
    

-}
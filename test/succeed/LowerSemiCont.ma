-- If F is anitone, then it is lower semi-continuous

cofun sup : (F : Size -> Set) +(i : Size) -> Set
{ sup F i = [j < i] & F j }

let pairF [F : -Size -> Set] (a : F #) : sup F #
  = (#, a)

-- [j < i] & F j  is lower semi in i
let supsup [F : Size -> Set] (a : sup F #) : sup (sup F) #
  = (#, a)

cofun bsup : (F : Size -> Set) +(i : Size) -> Set
{ bsup F i = [j <= i] & F j }

-- [j <= i] & F j  is lower semi in i if F i
let bsupsup [F : Size -> Set] (a : sup F #) : bsup (sup F) #
  = (#, a)

sized data SNat : +Size -> Set
{ szero : [i : Size] -> SNat $i
; ssuc  : [i : Size] -> SNat i -> SNat $i
}

let pairSNat (a : SNat #) : [j < #] & SNat j
  = (#, a)

let pairSNat2 (a : SNat #) : [j < #] & SNat j & SNat j
  = (#, a, a)

data Fork ++(A : Set)
{ fork (fst : A) (snd : A)
} fields fst, snd

-- tuples of lsc things are lsc
let forkSNat (a : SNat #) : [j < #] & Fork (SNat j)
  = (#, fork a a)

data Maybe ++(A : Set)
{ nothing
; just (fromJust : A)
} fields fromJust

let maybeSNat (a : SNat #) : [j < #] & Maybe (SNat j)
  = (#, just a)

data List ++(A : Set)
{ nil
; cons (x : A)(xs : List A)
}

fail -- inductive types preserve lcs, but not supported yet
let listSNat (a : SNat #) : [j < #] & List (SNat j)
  = (#, cons a nil)

data Nat +(i : Size) : Set
{ zero : Nat i
; suc  : (jn : [j < i] & Nat j) -> Nat i
}

let one : Nat # = suc (#,zero)

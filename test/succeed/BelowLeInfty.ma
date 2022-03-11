sized data Nat : +(i <= #) -> Set
{ zero [i <= #]             : Nat $i
; succ [i <= #] (n : Nat i) : Nat $i
}

let sib00 : ([i : Size] -> Nat i) -> ([i : Size] -> Nat i)  = \ x -> x
let sib01 : ([i : Size] -> Nat i) -> ([i <  #] -> Nat i)  = \ x -> x
let sib11 : ([i <  #] -> Nat i) -> ([i <  #] -> Nat i)  = \ x -> x

fail let sib10 : ([i <  #] -> Nat i) -> ([i : Size] -> Nat i)  = \ x -> x

let sub00 : ([i <= #] -> Nat i) -> ([i <= #] -> Nat i)  = \ x -> x
let sub01 : ([i <= #] -> Nat i) -> ([i <  #] -> Nat i)  = \ x -> x
let sub11 : ([i <  #] -> Nat i) -> ([i <  #] -> Nat i)  = \ x -> x

fail let sub10 : ([i <  #] -> Nat i) -> ([i <= #] -> Nat i)  = \ x -> x

let sub1 : ([i : Size] -> Nat i) -> ([i <= #] -> Nat i)
  = \ x -> x

let sub2 : ([i <= #] -> Nat i) -> ([i : Size] -> Nat i)
  = \ x -> x

sized data MNat : +(i <= #) -> Set
{ mzero [i : Size]            : MNat $i
; msucc [i <= #] (n : MNat i) : MNat $i
}

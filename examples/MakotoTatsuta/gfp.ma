-- 2010-08-18

sized data List (A : Set) : Size -> Set 
{ nil  : [i : Size] -> List A $i
; cons : [i : Size] -> A -> List A i -> List A $i
}

sized codata CoList (A : Set) : Size -> Set 
{ conil  : [i : Size] -> CoList A $i
; cocons : [i : Size] -> A -> CoList A i -> CoList A $i
}

fun coerce : [A : Set] -> [i : Size] -> List A i -> CoList A i
{ coerce A .$i (nil .A i) = conil A i
; coerce A .$i (cons .A i a as) = cocons A i a (coerce A i as)
}

cofun coerce' : [A : Set] -> [i : Size] -> List A i -> CoList A i
{ coerce' A ($i) (nil .A .i) = conil A i
; coerce' A ($i) (cons .A .i a as) = cocons A i a (coerce' A i as)
}

cofun repeat : [A : Set] -> (a : A) -> [i : Size] -> CoList A i
{ repeat A a ($ i) = cocons A i a (repeat A a i)
}

data Unit : Set { unit : Unit }
eval let units : CoList Unit # = repeat Unit unit #

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

sized data SNat : Size -> Set
{ zz : [i : Size] -> SNat $i
; ss : [i : Size] -> SNat i -> SNat $i
}

cofun repeatN : [A : Set] -> (a : A) -> [i : Size] -> SNat i -> CoList A i
{ repeatN A a ($ i) (zz .i)   = conil A i
; repeatN A a ($ i) (ss .i n) = cocons A i a (repeatN A a i n)
}

fun repeatN' : [A : Set] -> (a : A) -> [i : Size] -> SNat i -> CoList A i
{ repeatN' A a .($ i) (zz i)   = conil A i
; repeatN' A a .($ i) (ss i n) = cocons A i a (repeatN' A a i n)
}

sized codata Tree : Size -> Set
{ leaf : [i : Size] -> Tree $i
; node : [i : Size] -> Tree i -> Tree i -> Tree $i
}

fun mkTree : [i : Size] -> SNat i -> Tree i
{ mkTree .($i) (zz i)   = leaf i
; mkTree .($i) (ss i n) = node i (mkTree i n) (mkTree i n)
}

cofun mkTree' : [i : Size] -> SNat i -> Tree i
{ mkTree' ($i) (zz .i)   = leaf i
; mkTree' ($i) (ss .i n) = node i (mkTree' i n) (mkTree' i n)
}

sized data HasLength (A : Set) : (i : Size) -> CoList A # -> Nat -> Set
{ hasZero : [i : Size] -> HasLength A $i (conil A #) zero
; hasSucc : [i : Size] -> [a : A] -> [l : CoList A #] -> [n : Nat] ->
   HasLength A i l n -> HasLength A $i (cocons A # a l) (succ n)
}

fun conv : [A : Set] -> [i : Size] -> (l : CoList A #) -> (n : Nat) -> 
  HasLength A i l n -> List A #
{ conv A i (conil .A .#) zero (hasZero .A (i > j)) = nil A #
; conv A i (cocons .A .# a as) (succ n) (hasSucc .A (i > j) .a .as .n d) =
  cons A # a (conv A j as n d)
}

{-
fun conv : [A : Set] -> [i : Size] -> (l : CoList A i) -> (n : Nat) -> 
  HasLength A i l n -> List A i
{ conv A i (conil .A .j) zero (hasZero .A (i > j)) = nil A j
; conv A i (cocons .A .j a as) (succ n) (hasSucc .A (i > j) .a .as .n d) =
  cons A j a (conv A j as n d)
}
-} 

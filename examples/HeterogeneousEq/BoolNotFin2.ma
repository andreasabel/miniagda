{- 2010-09-21 Agda issue 292

see also BoolNotFin2.txt

-}

data Empty : Set {}

data Bool : Set
{ true  : Bool
; false : Bool
}

-- instead of Fin 2 I use another isomorphic copy of Bool
data Bool2 : Set
{ true2  : Bool2
; false2 : Bool2
}

data HetEq (A : Set 1)(a : A) : (B : Set 1) -> (b : B) -> Set
{ hrefl : HetEq A a A a
}

{- this won't get me far...
fun hsubst : (A, B : Set 1) -> (P : (S : Set 1) -> S -> Set) ->
            (a : A) -> (b : B) -> HetEq A a B b -> P A a -> P B b
{ hsubst A .A P a .a (hrefl .A .a .A .a) p = p
}
-}

fun subst : (A : Set 1) -> (P : A -> Set) ->
            (a, b : A) -> HetEq A a A b -> P a -> P b
{ subst A P a .a (hrefl) p = p
}

data Sigma (A : Set)(B : A -> Set) : Set
{ pair : (fst : A) -> (snd : B fst) -> Sigma A B
}

let T : (S : Set) -> (s : S) -> Set
  = \ S s -> HetEq S s Bool true

let P : Set -> Set
   = \ S -> Sigma S (T S)

let pBool : P Bool
  = pair true (hrefl)

-- here, we check whether  HetEq Bool2 true2 Bool true  is empty
fun notPBool2 : P Bool2 -> Empty
{ notPBool2 (pair true2  ())
; notPBool2 (pair false2 ())
}

let tada : HetEq Set Bool Set Bool2 -> Empty
 = \ h -> notPBool2 (subst Set P Bool Bool2 h pBool)

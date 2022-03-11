-- 2010-09-20

fun f : (A, B : Set 0) -> (A -> B) -> B {}
fun g : (A, B : Set 1) -> (A -> B) -> B {}
fun h : (A : Set 0) -> (B : Set 1) -> (A -> B) -> B {}
-- parametricity tells us
--  h : [A : Set 0] -> [B : Set 1] -> (A -> B) -> B {}
-- but MiniAgda only sees size

fun p : (i : Size) -> (A : Set i) -> A -> A {}
-- MiniAgda can only tell us that p can be in any class
-- need to do something smart for universe polymorphic things ...

data List (i : Size)(A : Set i) : Set i
{ nil  : List i A
; cons : A -> List i A -> List i A
}


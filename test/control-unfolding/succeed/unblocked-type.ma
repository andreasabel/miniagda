-- Level-polymorphic identity

fun TYPE : (i : Size) -> Set $i
{ TYPE i = Set i }

fun TPolyId : (i : Size) -> Set $i
unfold TYPE
{ TPolyId i = (A : TYPE i) -> A -> A }

fun polyId : (i : Size) -> TPolyId i
unfold TPolyId
{ polyId i A x = x }

-- Polymorphic identity

fun SET : Set 1
{ SET = Set }

fun TId0 : Set 1
unfold SET
{ TId0 = (A : SET) -> A -> A }

let SET0 : Set 1 = Set
let TId : Set 1 unfold SET0 = (A : SET0) -> A -> A

-- TODO
-- data Empty : unfold SET in SET
-- {}

-- 2013-03-30 constraints must follow quantifier
check
fun f : [A : Set] -> ([i : Size] -> |i| < |i| -> A) -> A {}

check
fun f : [A : Set] -> ([i : Size] -> |i| < |i| -> |i| < |i| -> A) -> A {}

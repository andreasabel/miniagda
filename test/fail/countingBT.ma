data BT : Set 
{ lf : BT
; node : BT -> BT -> BT
}

mutual {
 fun f : BT -> BT
 { f (node l (node rl rr)) = g l rr 
 }
 fun g : BT -> BT -> BT
 { g t u = f (node t u)
 }
}
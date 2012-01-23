data Nat : Set 
{ zero : Nat
; succ : Nat -> Nat
}

data List (+A : Set) : Set
{ nil : List A
; cons : A -> List A -> List A
}

-- de Bruijn terms

data Exp : Set
{ var : Nat -> Exp
; abs : Exp -> Exp 
; app : Exp -> Exp -> Exp
}

data D : Set
{ clos : Exp -> (List D) -> D
}

let Env : Set
      = List D

let empty : Env
      = nil

let update : Env -> D -> Env
      = \ rho -> \ d -> cons d rho       

let dummy : D
          = clos (var zero) empty

fun lookup : Env -> Nat -> D
{ lookup nil n = dummy
; lookup (cons d rho) zero = d
; lookup (cons d rho) (succ n) = lookup rho n
}

-- a sized type of evaluation trees

sized data Eval : Size -> Exp -> Env -> D -> Set
{ evVar : (i : Size) -> (k : Nat) -> (rho : Env) ->  
          Eval ($ i) (var k) rho (lookup rho k)
; evAbs : (i : Size) ->  (e : Exp) -> (rho : Env) -> 
          Eval ($ i) (abs e) rho (clos e rho)  
; evApp : (i : Size) -> (f : Exp) -> (e : Exp) -> (rho : Env) -> 
          (f' : Exp) -> (rho' : Env) -> (d : D) -> (d' : D) -> 
          Eval i f rho (clos f' rho') ->
          Eval i e rho d ->
          Eval i f' (update rho' d) d' ->
          Eval ($ i) (app f e) rho d'
}

mutual {

  fun evaluate : [i : Size] -> (e : Exp) -> (rho : Env) -> 
             [d : D] -> [Eval i e rho d] -> <d : D>
  { evaluate .($ i) (var k) rho .(lookup rho k) (evVar i .k .rho) = lookup rho k
  ; evaluate .($ i) (abs e) rho .(clos e rho)   (evAbs i .e .rho) = clos e rho
  ; evaluate .($ i) (app f e) rho .d' (evApp i .f .e .rho f' rho' d d' evF evE evF')
      = apply ($ i) f' rho' (evaluate i f rho (clos f' rho') evF)
                            (evaluate i e rho d evE) 
                            d' evF'
  }

  fun apply : [i : Size] -> [f' : Exp] -> [rho' : Env] -> <clos f' rho' : D> -> 
              (d : D) -> [d' : D] -> [Eval i f' (update rho' d) d'] -> <d' : D> 
  { apply i .f' .rho' (clos f' rho') d d' p = evaluate i f' (update rho' d) d' p  
  }
}



sized data CanEval : Size -> Exp -> Env -> Set
{ canVar : (i : Size) -> (k : Nat) -> (rho : Env) ->  
           CanEval ($ i) (var k) rho
; canAbs : (i : Size) ->  (e : Exp) -> (rho : Env) -> 
           CanEval ($ i) (abs e) rho
; canApp : (i : Size) -> (f : Exp) -> (e : Exp) -> (rho : Env) -> 
           (f' : Exp) -> (rho' : Env) -> (d : D) -> 
           Eval i f rho (clos f' rho') ->
           Eval i e rho d -> 
           CanEval i f' (update rho' d) ->
           CanEval ($ i) (app f e) rho 
}


fun can : [i : Size] -> (e : Exp) -> (rho : Env) -> [d : D] -> 
          Eval i e rho d -> CanEval i e rho
{ can .($ i) .(var k) .rho .(lookup rho k) (evVar i k rho) = canVar i k rho
; can .($ i) .(abs e) .rho .(clos e rho)   (evAbs i e rho) = canAbs i e rho
; can .($ i) .(app f e) .rho .d' (evApp i f e rho f' rho' d d' evF evE evF') =
    canApp i f e rho f' rho' d evF evE (can i f' (update rho' d) d' evF')
}

{-
fun evaluate : [i : Size] -> (e : Exp) -> (rho : Env) -> [CanEval i e rho] -> D
{ evaluate .($ i) (var k) rho (canVar i .k .rho) = lookup rho k
; evaluate .($ i) (abs e) rho (canAbs i .e .rho) = clos e rho
; evaluate .($ i) (app f e) rho (canApp i .f .e .rho f' rho' d evF evE canF') =
    apply (evaluate i f rho (can i f rho (clos f' rho') evF))
          (evaluate i e rho (can i e rho d evE))
} 
-}
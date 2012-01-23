-- 2009-11-29  A partial normalizer for untyped lambda calculus in MiniAgda

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

-- set of values

data D : Set
{ clos : Exp -> (List D) -> D
}

-- environment operations

let Env : Set
      = List D

let empty : Env
      = nil

let update : Env -> D -> Env
      = \ rho -> \ d -> cons d rho       

let dummy : D
          = clos (var zero) empty

fun lookup : Env -> Nat -> D
{ lookup (nil) n = dummy
; lookup (cons d rho) zero = d
; lookup (cons d rho) (succ n) = lookup rho n
}

-- inductive graph of the evaluation function

data Eval : Exp -> Env -> D -> Set
{ evVar : (k : Nat) -> (rho : Env) ->  

          -------------------------------
          Eval (var k) rho (lookup rho k)

; evAbs : (e : Exp) -> (rho : Env) -> 

          -----------------------------
          Eval (abs e) rho (clos e rho)  

; evApp : (f : Exp) -> (e : Exp) -> (rho : Env) -> 
          (f' : Exp) -> (rho' : Env) -> (d : D) -> (d' : D) -> 

          Eval f rho (clos f' rho') ->
          Eval e rho d ->
          Eval f' (update rho' d) d' ->
          -----------------------------  
          Eval (app f e) rho d'
}

-- evaluation as a partial function
{- after erasure, the function takes the form

    evaluate : Exp -> Env -> D
-}

mutual {

  fun evaluate : (e : Exp) -> (rho : Env) -> 
                 [d : D] -> [Eval e rho d] -> <d : D>

  { evaluate (var k) rho .(lookup rho k) (evVar .k .rho) = lookup rho k
  ; evaluate (abs e) rho .(clos e rho)   (evAbs .e .rho) = clos e rho
  ; evaluate (app f e) rho .d' (evApp .f .e .rho f' rho' d d' evF evE evF')
      = apply f' rho' (evaluate f rho (clos f' rho') evF)
                      (evaluate e rho d evE)  d' evF'
  }

  fun apply : [f' : Exp] -> [rho' : Env] -> <clos f' rho' : D> -> 
              (d : D) -> [d' : D] -> [Eval f' (update rho' d) d'] -> <d' : D> 
  { apply .f' .rho' (clos f' rho') d d' p = evaluate f' (update rho' d) d' p  
  }
}


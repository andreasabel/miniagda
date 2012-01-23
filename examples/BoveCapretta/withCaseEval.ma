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
{ lookup (nil) n = dummy
; lookup (cons d rho) zero = d
; lookup (cons d rho) (succ n) = lookup rho n
}

-- a sized type of evaluation trees

data Eval : Exp -> Env -> D -> Set
{ evVar : (k : Nat) -> (rho : Env) ->  
          Eval (var k) rho (lookup rho k)
; evAbs : (e : Exp) -> (rho : Env) -> 
          Eval (abs e) rho (clos e rho)  
; evApp : (f : Exp) -> (e : Exp) -> (rho : Env) -> 
          (f' : Exp) -> (rho' : Env) -> (d : D) -> (d' : D) -> 
          Eval f rho (clos f' rho') ->
          Eval e rho d ->
          Eval f' (update rho' d) d' ->
          Eval (app f e) rho d'
}

trustme
fun evaluate : (e : Exp) -> (rho : Env) -> 
                 [d : D] -> [Eval e rho d] -> <d : D>
{ evaluate (var k) rho .(lookup rho k) (evVar .k .rho) = lookup rho k
; evaluate (abs e) rho .(clos e rho)   (evAbs .e .rho) = clos e rho
; evaluate (app f e) rho .d' (evApp .f .e .rho f' rho' d d' evF evE evF') =
    case (evaluate f rho (clos f' rho') evF) 
    { (clos f' rho') -> evaluate f' (update rho' (evaluate e rho d evE))  d' evF'
    }
}


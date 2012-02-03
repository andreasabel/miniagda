-- stl.ma  Simply Typed Lambda calculus, implemented with de Bruijn indices

-- Types are unlabeled binary trees

let Ty = BinTree Unit
pattern base = leaf
pattern arrow a b = node a unit b

let arr [i : Size] (a, b : Ty i) : Ty $i
  = arrow (i, a) (i, b)

-- Contexts are lists of types
let Context = List (Ty #)

let extend [i : Size] (a : Ty #) (cxt : Context i) : Context $i
  = consL (Ty #) i a cxt

-- Well-typed variables

fun Var : [i : Size] -> |i| -> (cxt : Context i) -> ^(c : Ty #) -> Set
{ Var i nil               c = Empty
; Var i (cons a (j, cxt)) c = Either (Id (Ty #) a c) (Var j cxt c)
} 

-- Variables are a variant of natural numbers
pattern vzero   = left refl
pattern vsucc x = right x

let vzer (cxt : Context #) (a : Ty #) : Var # (extend # a cxt) a 
  = vzero

let vsuc (cxt : Context #) (a, b : Ty #) (x : Var # cxt a) 
  : Var # (extend # b cxt) a
  = vsucc x

-- Well-typed terms

cofun Term :  +(i : Size) -> (cxt : Context #) -> (c : Ty #) -> Set
{ Term i cxt c = 
    let ++T (cxt : Context #) (c : Ty #) = [j < i] & Term j cxt c 
    in  Tri (Var # cxt c)                              -- var
            ((a : Ty #) & T cxt (arr # a c) & T cxt a) -- app
            (case c                                    -- abs
             { (base)                -> Empty
             ; (arrow (j, a) (k, b)) -> T (extend # a cxt) b 
             })
}
pattern var x     = first x
pattern app a t u = second (a, t, u)
pattern abs t     = third t

-- Example terms

pattern v0 = vzero
pattern v1 = vsucc v0
pattern v2 = vsucc v1
pattern v3 = vsucc v2

pattern var0 = var v0
pattern var1 = var v1
pattern var2 = var v2
pattern var3 = var v3

let tyId : Ty # = arr # base base
let tmId : Term # nil tyId = abs (0, var0) 

let tyK : Ty # = arr # base tyId
let tmK : Term # nil tyK = abs (1, abs (0, var1))

let tyS : Ty # = arr # tyK (arr # tyId tyId)
let tmS : Term # nil tyS = abs (4, abs (3, abs (2, app base
  (1, app base (0, var2) (0, var0)) 
  (1, app base (0, var1) (0, var0)))))

-- Renamings

let Renaming (gamma, delta : Context #)
  = (a : Ty #) -> Var # delta a -> Var # gamma a

check
fun liftRen : (gamma, delta : Context #) -> (c : Ty #) -> 
  (rho : Renaming gamma delta) -> Renaming (extend # c gamma) (extend # c delta)
{ liftRen gamma delta c rho .c vzero    = vzero
; liftRen gamma delta c rho a (vsucc x) = vsucc (rho a x) 
}

let liftRen (gamma, delta : Context #) (c : Ty #) (rho : Renaming gamma delta) 
  : Renaming (extend # c gamma) (extend # c delta)
  = \ a y -> case y
    { (left p)  -> left p
    ; (right x) -> right (rho a x)
    }

fun rename : (gamma, delta : Context #) -> (c : Ty #) -> 
  [i : Size] -> Term i delta c -> Renaming gamma delta -> Term i gamma c
{ rename gamma delta c i (var x)             rho = var (rho c x)
; rename gamma delta c i (app a (j,t) (k,u)) rho = 
    app a (j, rename gamma delta (arr # a c) j t rho)
          (k, rename gamma delta  a          k u rho)
; rename gamma delta base                  i (abs ())    rho 
; rename gamma delta (arrow (k1,a) (k2,b)) i (abs (j,t)) rho = 
    abs (j, rename (extend # a gamma) (extend # a delta) b j t 
              (liftRen gamma delta a rho))
}

-- Substitutions

let Substitution +(i : Size) (gamma, delta : Context #)
  = (a : Ty #) -> Var # delta a -> Term i gamma a

let liftSubst (gamma, delta : Context #) (c : Ty #) 
      [i : Size] (sigma : Substitution i gamma delta) 
  : Substitution i (extend # c gamma) (extend # c delta)
  = \ a y -> case y
    { (left p)  -> var (left p)
    ; (right x) -> rename (extend # c gamma) gamma a i 
                     (sigma a x) (\ b x -> vsucc x)
    }

fun substitute : (gamma, delta : Context #) -> (c : Ty #) -> 
  [i : Size] -> |i| -> Term i delta c -> 
  [j : Size] -> Substitution j gamma delta -> Term (i+j) gamma c
{ substitute gamma delta c i (var x) j sigma = sigma c x
; substitute gamma delta c i (app a (i1, t) (i2, u)) j sigma =
    app a (i1+j, substitute gamma delta (arr # a c) i1 t j sigma)
          (i2+j, substitute gamma delta a           i2 u j sigma)
; substitute gamma delta base                    i (abs ())      j sigma 
; substitute gamma delta (arrow (k1, a) (k2, b)) i (abs (i', t)) j sigma =
    abs (i' + j, substitute (extend # a gamma) (extend # a delta) b i' t j 
                   (liftSubst gamma delta a j sigma))
}

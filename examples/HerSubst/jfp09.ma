-- 2010-07-27 Implementation of JFP-paper
-- Implementing a Normalizer Using Heterogeneous Sized Types

data Maybe (A : Set) : Set
{ nothing : Maybe A
; just    : A -> Maybe A
}

fun mapMaybe : [A, B : Set] -> (A -> B) -> Maybe A -> Maybe B
{ mapMaybe A B f (nothing .A) = nothing B
; mapMaybe A B f (just .A a)  = just B (f a)
}

sized data Ty : Size -> Set 
{ base : [i : Size] -> Ty $i
; arr  : [i : Size] -> Ty i -> Ty i -> Ty $i
}

sized data Tm (A : Set) : Size -> Set
{ var  : [i : Size] -> A -> Tm A $i
; app  : [i : Size] -> Tm A i -> Tm A i -> Tm A $i
; abs  : [i : Size] -> Ty # -> Tm (Maybe A) i -> Tm A $i
}

fun mapTm : [A, B : Set] -> [i : Size] -> |i| -> (A -> B) -> Tm A i -> Tm B i
{ mapTm A B i f (var .A (j < i) x)   = var B j (f x)
; mapTm A B i f (app .A (j < i) r s) = app B j (mapTm A B j f r) (mapTm A B j f s)
; mapTm A B i f (abs .A (j < i) a r) = 
    abs B j a (mapTm (Maybe A) (Maybe B) j (mapMaybe A B f) r)
}

let shiftTm : [A : Set] -> [i : Size] -> Tm A i -> Tm (Maybe A) i
  = \ A i t -> mapTm A (Maybe A) i (just A) t

-- result of substitution is carrying a type or not
data Res (A : Set) +(i : Size) : Set
{ ne : Tm A # -> Res A i
; nf : Tm A # -> Ty i -> Res A i
}

fun tm : [A : Set] -> [i : Size] -> Res A i -> Tm A #
{ tm A i (ne .A .i t)   = t
; tm A i (nf .A .i t a) = t
}

fun shiftRes : [A : Set] -> [i : Size] -> Res A i -> Res (Maybe A) i
{ shiftRes A i (ne .A .i t)   = ne (Maybe A) i (shiftTm A # t)
; shiftRes A i (nf .A .i t a) = nf (Maybe A) i (shiftTm A # t) a
}

-- construct results without type information
let varRes : [A : Set] -> [i : Size] -> A -> Res A i
  = \ A i x -> ne A i (var A # x)

let absRes : [A : Set] -> [i : Size] -> Ty # -> Res (Maybe A) # -> Res A i
  = \ A i a r -> ne A i (abs A # a (tm (Maybe A) # r))

let appRes : [A : Set] -> [i : Size] -> Res A # -> Res A # -> Res A i
  = \ A i t u -> ne A i (app A # (tm A # t) (tm A # u))

-- environments (in paper: Val)

let Env : Set -> Set -> Size -> Set
  = \ A B i -> A -> Res B i

fun sg : [A : Set] -> [i : Size] -> Tm A # -> Ty i -> Env (Maybe A) A i
{ sg A i s a (nothing .A) = nf A i s a
; sg A i s a (just .A y)  = varRes A i y
}

fun lift : [A, B : Set] -> [i : Size] -> Env A B i -> Env (Maybe A) (Maybe B) i
{ lift A B i rho (nothing .A) = varRes (Maybe B) i (nothing B)
; lift A B i rho (just .A x)  = shiftRes B i (rho x)
} 

-- hereditary substitution

mutual {

  fun subst : [i : Size] -> |i,$$0,#| -> Ty i -> 
              [A : Set] -> Tm A # -> Tm (Maybe A) # -> Tm A #
  { subst i a A s t = tm A i (simsubst i # (Maybe A) A t (sg A i s a))
  }  
  
  fun simsubst : [i, j : Size] -> |i,$0,j| -> 
                 [A, B : Set] -> Tm A j -> Env A B i -> Res B i
  { simsubst i j A B (var .A (j' < j) x) rho = rho x
  ; simsubst i j A B (abs .A (j' < j) b t) rho = 
      absRes B i b (simsubst i j' (Maybe A) (Maybe B) t (lift A B i rho)) 
  ; simsubst i j A B (app .A (j' < j) t u) rho =
      let t' : Res B i = simsubst i j' A B t rho in
      let u' : Res B i = simsubst i j' A B u rho in
        normApp i B t' u'
  {- alternative: case
        case t'
        { (nf .B .i (abs .B .# b' r') (arr (i > i') b c)) ->
            nf B i' (subst i' b (Maybe B) (tm u') r') c
        ; bla -> appRes B i t' u' 
        }
  -}
  }
  
  fun normApp : [i : Size] -> |i,0,#| -> 
                [B : Set] -> Res B i -> Res B i -> Res B i
  { normApp i B (nf .B .i (abs .B .# b' r') (arr (i' < i) b c)) u' =
      nf B i' (subst i' b B (tm B i u') r') c
  ; normApp i B t' u' = appRes B i t' u'
  }

}

-- normalization

fun norm : [i : Size] -> |i| -> [A : Set] -> Tm A i -> Tm A #
{ norm i A (var .A (i' < i) x) = var A # x
; norm i A (abs .A (i' < i) a t) = abs A # a (norm i' (Maybe A) t)
; norm i A (app .A (i' < i) t u) = 
   let t' : Tm A # = norm i' A t in
   let u' : Tm A # = norm i' A u in
     case t' 
     { (abs .A .# a r) -> subst # a A u' r
     ; bla -> app A # t' u'
     }
}

data Empty : Set {}

let tI : Ty # -> Tm Empty #
  = \ a -> abs Empty # a (var (Maybe Empty) # (nothing Empty))

let k0 : Ty # = base #
let k1 : Ty # = arr # k0 k0

let tII : Tm Empty #
  = app Empty # (tI k1) (tI k0) 

eval let nII : Tm Empty #
  = norm # Empty tII  -- identity

eval let nII' : Tm Empty #
  = norm # Empty (app Empty # (tI k0) (tI k0)) -- also identity

eval let nIII' : Tm Empty #
  = norm # Empty (app Empty # (app Empty # (tI k0) (tI k0)) (tI k0)) -- also identity



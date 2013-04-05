-- 2012-02-05  Check whether we can define dependent case in MiniAgda
-- 2013-04-02  Musings on fixed-point

let Map (F : Set -> Set)
  = [A, B : Set] -> (A -> B) -> F A -> F B

cofun Nu : (F : Set -> Set) -(i : Size) -> Set
{ Nu F i = [j < i] -> F (Nu F j)
}

-- * we have Nu F # <==> [i < #] -> Nu F i

cofun Inf : (G : Size -> Set) -(i : Size) -> Set
{ Inf G i = [j < i] -> G j }

let usc [F : Set -> Set] (r : Inf (Nu F) #) : Nu F #
  = r # -- uses upper semi cont

cofun toInf : (F : Set -> Set) (r : Nu F #) -> Inf (Nu F) #
{ toInf F r i j = r j }

-- * we also have Nu F # <==> [i <= #] -> Nu F i

let All (G : Size -> Set) = [i : Size] -> G i

let fromAll [F : Set -> Set] (r : All (Nu F)) : Nu F #
  = r #  -- trivial

cofun toAll : (F : Set -> Set) (r : Nu F #) -> All (Nu F)
{ toAll F r i j = r j }

-- post-fixed point
-- the reasoning usually is
-- Nu F # = Nu F $# = [j < $#] -> F (Nu F j) ==> F (Nu F #)
fail -- 2013-04-05 should work, but needs implementation
fun postfp : [F : Set -> Set] (r : Nu F #) -> F (Nu F #)
{ postfp F r = r # }

-- destructor

let out [F : Set -> Set] [i : Size] (r :  Nu F $i) : F (Nu F i)
  = r i
-- fails to typecheck #ifdef STRICTINFTY (would succeed if i<#)
-- r : [j < $i] -> F (Nu F j)
-- r i : |i| < |$i| -> F (Nu F i)

-- constructor (needs monotonicity of F)

check
fun inn : [F : +Set -> Set] [i : Size] -> F (Nu F i) -> Nu F $i
{ inn F i t j = t
}

let inn [F : +Set -> Set] [i : Size] (t : F (Nu F i)) : Nu F $i
  = \ j -> t

-- coiteration
-- 2013-03-30 this must be a cofun, since not SN.
cofun coit : [F : +Set -> Set] (map : Map F)
  [S : Set] (step : S -> F S)
  [i : Size] -> |i| -> (start : S) -> Nu F i
{ coit F map S step i
    = \ start j -> map S (Nu F j) (coit F map S step j) (step start)
}

{- not needed (eta is built-in)
-- eta

let eta [F : +Set -> Set] [i : Size] (r : Nu F $i) : Nu F $i
  = \ j -> r j

fun caseNu : [F : +Set -> Set]
  [P : (i : Size) -> Nu F i -> Set]
  (f : [i : Size] -> (t : F (Nu F i)) -> P $i (inn F i t))
  [i : Size] (x : Nu F $i) -> P $i (eta F i x)
{ caseNu F P f i x = f i (x i)
}
-}

-- case

let caseNu
  [F : +Set -> Set]
  [P : (i : Size) -> Nu F i -> Set]
  (f : [i : Size] -> (t : F (Nu F i)) -> P $i (inn F i t))
  [i : Size]
  (x : Nu F $i) : P $i x
                = f i (x i)

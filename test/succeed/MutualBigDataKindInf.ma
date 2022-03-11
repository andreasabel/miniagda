-- 2010-09-20

data Unit : Set { unit : Unit }
mutual {

  data MaybeBig : Set 1
  { Nothing : MaybeBig
  ; Just    : Unit -> Big -> MaybeBig
  }

  data Big : Set 1
  { BigIn : (BigOut : Set) -> Big
  } fields BigOut

}

fun Maybe : MaybeBig -> Set -> (Set -> Set) -> Set
{ Maybe Nothing A F = A
; Maybe (Just u B) A F = F (BigOut B)
}

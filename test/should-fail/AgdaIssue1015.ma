-- 2014-01-09

mutual {
  data D -(i : Size)
  { inn (out : R i) }

  data R -(i : Size)
  { delay (force : [j < i] -> D j)
  } fields force
}

fun inh : [i : Size] -> R i
{ inh i .force j = inn (inh j)
}

data Empty : Set {}

fun elim : D # -> Empty
{ elim (inn r) = elim (r .force #)
}

-- OLD comment:
-- Stack overflow because MiniAgda thinks D and R are not recursive
-- and does eta-expansion into all eternity

let absurd : Empty = elim (inn (inh #))

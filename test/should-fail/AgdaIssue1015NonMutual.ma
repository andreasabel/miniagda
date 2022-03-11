-- 2014-01-09
-- Treating a type as both inductive and coinductive bears trouble...

data R -(i : Size)
{ delay (force : [j < i] -> R j)
} fields force

-- Coinductive interpretation:

fun inh : [i : Size] -> R i
{ inh i .force j = inh j
}

data Empty : Set {}

-- Inductive interpretation:

fun elim : R # -> Empty
{ elim (delay r) = elim (r #)
}

-- Trouble:

let absurd : Empty = elim (inh #)


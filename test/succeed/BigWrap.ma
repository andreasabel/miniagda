-- 2010-09-20 big data type

data BigWrap : Set 1
{ inn : (out : Set) -> BigWrap
}

-- 2012-10-10: automatic irrelevance analysis (forcing)
-- turns this into [A : Set] -> NotBig A
data NotBig : Set -> Set
{ notBig : (A : Set) -> NotBig A
}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

data NAT : Set 1
{ ZERO : NAT
; SUCC : NAT -> NAT
}

fun NATnat : NAT -> Nat
{ NATnat ZERO = zero
; NATnat (SUCC n) = succ (NATnat n)
}

-- small kind
data Exists : Set
{ inEx : [A : Set] -> (outEx : A) -> Exists
}

-- big kind
data EXISTS : Set 1
{ inEX : (OutType : Set) -> (outValue : OutType) -> EXISTS
}


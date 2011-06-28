-- 2010-09-20 big data type

data BigWrap : Set 1
{ inn : (out : Set) -> BigWrap
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
data Exists : Set 1
{ inEx : [A : Set] -> (outEx : A) -> Exists
}

-- big kind
data EXISTS : Set 1
{ inEX : (OutType : Set) -> (outValue : OutType) -> EXISTS
}


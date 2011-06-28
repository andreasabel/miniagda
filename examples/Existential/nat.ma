data Maybe ++(A : Set) : Set
{ nothing : Maybe A
; just    : A -> Maybe A
}

-- sized type and constructors

cofun Nat : +Size -> Set
{ Nat i = (j < i : Size) & Maybe (Nat j)
}

fun zero : (i : Size) -> Nat $i
{ zero i = (i , nothing (Nat i))
}

fun succ : (i : Size) -> Nat i -> Nat $i
{ succ i n = (i, just (Nat i) n)
}

-- fixed-point and constructors

let NAT : Set = (i : Size) & Nat i

fun Zero : NAT
{ Zero = ($0, zero 0)
}

fun Succ : NAT -> NAT
{ Succ (i, n) = ($i, succ i n)
}

fun plus : {i : Size} -> Nat i -> NAT -> NAT
{ plus i (j , nothing (Nat j)) m = m
; plus i (j , just (Nat j) n)  m = Succ (plus j n m)
}
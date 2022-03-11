data Succ : Set
{ succ : Succ -> Succ
}

fun homatch : (Succ -> Succ) -> Set
{ homatch succ = Succ
}

{-
data Lim (A : Set) : Set
{ lim : (A -> Lim A) -> Lim A
}

fun bla : (A : Set) -> Lim A ->
-}

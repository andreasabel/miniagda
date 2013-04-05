-- bug reported 2012-02-17

data Id ++(A : Set) (x : A) : A -> Set
{ refl : Id A x x
}

data Either ++(A, B : Set) : Set
{ left  : A -> Either A B
; right : B -> Either A B
}

cofun P : ++(A : Set) -> Set
{ P A = Either A A
}

fun Foo : ++(A : Set) -> P A -> Set
{ Foo A x = (z : A) & Id (P A) x (left z)
}

fun foo : ++(A : Set) -> (x : P A) -> Foo A x
{ foo A (left x) = (x, refl)
}

{-
/// leqVal' [(x,1),(A,0)] |- left x  <=^  left x : P A
/// conType left: expected P A to be a data type

P is a cofun (and in my original code it is actually corecursive). Is
MiniAgda too lazy here?

A: do not know, it works (2012-03-06)
-}

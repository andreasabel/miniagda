-- 2013-04-06

fail
fun Bla1 : (A, A : Set) -> Set
{ Bla1 A B = A
}

fail
fun Bla2 : (A, B : Set) -> Set
{ Bla2 = \ A A -> A
}

fail
fun Bla1' : (A : Set) -> (A : Set) -> Set
{ Bla1' A B = A
}

check
let Bla3 (A : Set) : (A : Set) -> Set = \ B -> A

fail
let Bla3 (A : Set) (A : Set) : Set = A

check
let Bla4 (A : Set) : Set -> Set = \ A -> A

fail
let Bla5 : Set -> Set -> Set = \ A A -> A

fail
let Bla6 : Set -> Set -> Set1 = \ A A -> Set

fail
let Hurz : Set = \ M i s s i s s i p p i -> i

check
let Bla7 (A : Set) : Set =
  let A = A in A

let Bla7 (A : Set) : Set =
  let A = A in
  let A = A in A

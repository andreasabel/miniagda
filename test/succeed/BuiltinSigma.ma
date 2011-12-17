-- 2011-12-17
-- non-dependent pairs

fun fst' : (A, B : Set) -> (A & B) -> A
{ fst' A B (a, b) = a
}

fun snd' : (A, B : Set) -> A & B -> B
{ snd' A B (a, b) = b
}

let swap : (A, B : Set) -> A & B -> B & A
  = \ A B p -> (snd' A B p, fst' A B p)

fun reassoc' : (A, B, C : Set) -> (A & B) & C -> A & (B & C)
{ reassoc' A B C ((a , b) , c) = let bc : B & C = b , c in a , bc  -- a , (b , c) -- parse err
}

fun reassoc'' : (A, B, C : Set) -> (A & B) & C -> A & (B & C)
{ reassoc'' A B C ((a , b) , c) = a , b , c  -- a , (b , c) -- parse err
}

-- dependent pairs

fun fst : (A : Set) -> (B : A -> Set) -> (x : A) & B x -> A
{ fst A B (a, b) = a
}

fun snd : (A : Set) -> (B : A -> Set) -> (p : (x : A) & B x) -> B (fst A B p)
{ snd A B (a, b) = b
}

let curry : (A : Set) -> (B : A -> Set) -> (C : (x : A) -> B x -> Set) -> 
   ((p : (x : A) & B x) -> C (fst A B p) (snd A B p)) -> 
   ((x : A) -> (y : B x) -> C x y) 
  = \ A B C f x y -> f (x , y)

fun uncurry : (A : Set) -> (B : A -> Set) -> (C : (x : A) -> B x -> Set) -> 
  ((x : A) -> (y : B x) -> C x y) -> 
  (p : (x : A) & B x) -> C (fst A B p) (snd A B p)
{ uncurry A B C f (x , y) = f x y
}

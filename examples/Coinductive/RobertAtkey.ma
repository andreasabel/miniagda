-- 2012-01-27 Example taken from
--
-- How to be a Productive Programmer
-- talk by Robert Atkey, 2011-11-11

-- MiniAgda Prelude

data Unit : Set { unit }

data Bool : Set { true; false }
fun If : (b : Bool) -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

let Maybe ++(A : Set) : Set
  = (b : Bool) & If b A Unit
pattern nothing = (false, unit)
pattern just a  = (true, a)

cofun Nat : +Size -> Set
{ Nat i = [j < i] & Maybe (Nat j)
}
pattern zero j   = (j, nothing)
pattern succ j n = (j, just n)
let suc [i : Size] (n : Nat i) : Nat $i = succ i n
  

-- Number Streams

cofun Str : -Size -> Set
{ Str i = [j < i] -> Nat # & Str j
}
pattern cons n ns = (n, ns)

cofun mergef : ([i : Size] -> Nat # -> Nat # -> Str i -> Str $i) ->
  [i : Size] -> Str i -> Str i -> Str i
{ mergef f i s1 s2 (j < i) = case (s1 j, s2 j)
  { (cons x xs, cons y ys) -> f j x y (mergef f j xs ys) j }
} 

-- "A bad choice of argument yields non-productive definitions"
fail
let test_badf = mergef (\ i x y s -> s)

cofun map : (f : Nat # -> Nat #) -> [i : Size] -> Str i -> Str i
{ map f i s (j < i) = case s j
  { (cons x xs) -> cons (f x) (map f j xs)
  }
}
let scons [i : Size] (x : Nat #) (xs : Str i) : Str $i
  = \ j -> cons x xs

-- productive for
let test_f  = mergef (\ i x y s -> scons i x (map (suc #) i s))
let test_f' = mergef (\ i x y s j -> cons x (map (suc #) i s))

-- Clock Variables
----------------------------------------------------------------------
{-
-- "Tomorrow": the |> operator 
let Tri +(S : Size -> Set) -(k : Size) : Set
  = [j < k] -> S j

-- Front Streams: the head is always visible

cofun Stream : -(k : Size) -> |k| -> Set
{ -- Stream k = Nat# & ([j < k] -> Stream j)
Stream i = Nat# & Tri Stream i -- termination check fails
}

-- "|>^k removes one unit of time remaining in k"
let delay [A : -Size -> Set] [k : Size] : A k -> Tri A k -- waste
  = \ a j -> a
-- Comment: delay wastes one unit, more precise is  Tri A $k
--          Should be called "waste"
-- Need A to be contravariant here

let Arr -(A : Size -> Set) +(B : Size -> Set) : Size -> Set
  = \ i -> A i -> B i

let app [k : Size] [A, B : Size -> Set] 
    (f : Tri (Arr A B) k) (a : Tri A k) : Tri B k
  = \ j -> f j (a j)

fun fix : [A : Size -> Set] -> (f : [j : Size] -> Tri A j -> A j) -> 
  [k : Size] -> |k| -> A k
{ fix A f i = f i (fix A f)
}
-}

-- "Tomorrow": the |> operator 
let Tri -(k : Size) +(S : (j < k) -> Set) : Set
  = [j < k] -> S j

-- Front Streams: the head is always visible

cofun Stream : -(k : Size) -> |k| -> Set
{ -- Stream k = Nat# & ([j < k] -> Stream j)
Stream i = Nat# & Tri i Stream -- termination check fails
}

-- "|>^k removes one unit of time remaining in k"
let delay [A : -Size -> Set] [k : Size] : A k -> Tri k A -- waste
  = \ a j -> a
-- Comment: delay wastes one unit, more precise is  Tri $k A
--          Should be called "waste"
-- Need A to be contravariant here

let Arr -(A : Size -> Set) +(B : Size -> Set) : Size -> Set
  = \ i -> A i -> B i

let app [k : Size] [A, B : Size -> Set] 
    (f : Tri k (Arr A B)) (a : Tri k A) : Tri k B
  = \ j -> f j (a j)

fun fix : [A : Size -> Set] -> (f : [j : Size] -> Tri j A -> A j) -> 
  [k : Size] -> |k| -> A k
{ fix A f i = f i (fix A f)
}

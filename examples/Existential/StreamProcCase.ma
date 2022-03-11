-- 2012-01-23 Stream processors

-- * prelude

record Unit : Set
{ unit : Unit
}

-- * Booleans

data Bool : Set
{ true  : Bool
; false : Bool
}

fun If : Bool -> ++(A, B : Set) -> Set
{ If true  A B = A
; If false A B = B
}

-- * disjoint sum

let Either ++(A, B : Set) : Set
  = (b : Bool) & If b A B

pattern left  a = (true  , a)
pattern right b = (false , b)

-- * pairs

fun fst : [A, B : Set] -> A & B -> A
{ fst A B (a , b) = a
}

fun snd : [A, B : Set] -> A & B -> B
{ snd A B (a , b) = b
}

fun mapPair :
  [A1, A2 : Set] -> (A1 -> A2) ->
  [B1, B2 : Set] -> (B1 -> B2) -> A1 & B1 -> A2 & B2
{ mapPair A1 A2 f B1 B2 g (a , b) = f a , g b
}

fun mapFst : [A1, A2 : Set] -> (A1 -> A2) -> [B : Set] -> A1 & B -> A2 & B
{ mapFst A1 A2 f B (a , b) = f a , b
}

fun mapSnd : [A, B1, B2 : Set] -> (B1 -> B2) -> A & B1 -> A & B2
{ mapSnd A B1 B2 g (a , b) = a , g b
}

-- * streams defined recursively

cofun Stream : ++(A : Set) -> -(i : Size) -> Set
{ Stream A i = [j < i] -> A & (Stream A j)
}

pattern cons x xs = x , xs

cofun build : [A : Set] -> [S : -Size -> Set] ->
  ([j : Size] -> S $j -> A & (S j)) -> [i : Size] -> S i -> Stream A i
{ build A S step i start (j < i) = case step j start
  { (a , s) -> a , build A S step j s
  }
}

let map [A, B : Set] (f : A -> B) : [i : Size] -> Stream A i -> Stream B i
 = build B (Stream A) (\ j as -> mapFst A B f (Stream A j) (as j))

let repeat [A : Set] (a : A) [i : Size] : Stream A i
  = build A (\ j -> Unit) (\ j u -> a , u) i unit

cofun repeat' : [A : Set] -> A -> [i : Size] -> Stream A i
{ repeat' A a i (j < i) = a , repeat' A a j
}

-- * Infinity streams

let head [A : Set] (s : Stream A #) : A
  = fst A (Stream A 0) (s 0)

cofun tail' : [A : Set] -> [i : Size] -> Stream A $i -> Stream A i
{ tail' A i s = snd A (Stream A i) (s i)
}

let tail [A : Set] (s : Stream A #) : Stream A #
  = tail' A # s

cofun tail_ : [A : Set] -> Stream A # -> Stream A #
{ tail_ A s (j < #) = snd A (Stream A $j) (s $j) j
}
cofun tail_1 : [A : Set] -> Stream A # -> Stream A #
{ tail_1 A s j = snd A (Stream A $j) (s $j) j
}

let split' [A : Set] [i : Size] (s : Stream A $i) : A & (Stream A i)
  = s i

let split [A : Set] : (s : Stream A #) -> A & (Stream A #)
  = split' A #

-- unlike tail_ cannot implement split directly
-- CHECK FOR omega-instantiation needed if [i < #] ->
-- should be instantiated with #



-- * Stream processors

fun A : Set {}
fun B : Set {}

cofun SP' : ++(X : Set) -> +(i : Size) -> Set
{ SP' X i = [j < i] & Either (A -> SP' X j) X
}

pattern get f = left f
pattern out x = right x

cofun SP : -(i : Size) -> Set
{ SP i = [j < i] -> B & (SP' (SP j) #)
}

pattern put b sp = b , sp

fun run' : [i : Size] -> (SP i -> Stream A # -> Stream B i) ->
           [j : Size] -> SP' (SP i) j -> Stream A # -> Stream B i
{ run' i r j (k < j , get f)  as = run' i r k (f (head A as)) (tail A as)
; run' i r j (k < j , out sp) as = r sp as
}

cofun run : [i : Size] -> SP i -> Stream A # -> Stream B i
{ run i sp as (j < i) = case sp j
  { (put b sp) -> b , run' j (run j) # sp as
  }
}


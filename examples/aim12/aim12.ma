{- Andreas Abel, LMU Munich             AIM-12, 2 Sep 2010, Nottingham, UK

           MiniAgda - Towards a Core Language for Agda
           ===========================================

                   Elaboration
Surface Language ---------------> Core Language
   (Agda 2)                  (PiSigma, MiniAgda, MiniTT)

Desired Features of Core Language:

- small language
- small (independent) type checker
- "simple" translation from surface language
  preserve computational complexity / efficiency
- intermediate language for compilation
- full formalization/verification

Core Language CIC (Coq):

- small type checker, but:
* positivity inference
* termination inference (guardedness)
* universe inference
- too clunky, no full formalization

PiSigma:

- very small language, based on mutual general recursion
- small type checker
- universes/termination/positivity not checked

MiniAgda today:

- sized data and codata types (NEW: mutual)
- positivity check
- NEW: first class polarities
- mutual recursion with size-change termination
- NEW: corecursion into tuples of coinductive types
- NEW: mutual recursion with measure termination
- NEW: bounded quantification
- NEW: universe polymorphism

Future plans:

- develop a simple core language TypeCore
- beautify surface language: hidden arguments...
- turn positivity and termination check into elaborators
- purpose of MiniAgda: proof of concept for sized types
- purpose of TypeCore: possible core language for Agda

TypeCore:

- mutual data/codata
- first-class sizes
- first-class polarities
- mutual recursion with measure termination
- explicit universe-polymorphism
- case instead of pattern matching
-}

{-
     Explicit Polarities
     ===================
-}

let Const : ++Set -> .Set -> Set
          = \ A -> \ X -> A

let DNeg : Set -> +Set -> Set
         = \ B -> \ A -> (A -> B) -> B

data PTree -(B : Set) ++(A : Set) : Set
{ pleaf : PTree B A
; pnode : A -> (B -> PTree B A) -> PTree B A
}

data Mu ++(F : ++Set -> Set) : Set
{ inn : F (Mu F) -> Mu F
}

{-   Computing Polarities
     ====================        Composition (AC)

            .        = zero       . p = .
         ++          = one       ++ p = p
          +   -                   + p = p  (p not ++)
            o                     o p = o  (p not .)
                                  - - = +

  Semiring (plus = infimum, times = composition)
-}




{-
    Corecursion into tupels
    =======================

Corecursion using size:

  f : [i : Size] -> .... -> C i

ok if

  C 0 = Top

-}

data Prod ++(A, B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B
}
fields fst, snd

sized codata Stream ++(A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A ($ i)
}
fields head, tail

sized codata Tree ++(A : Set) : Size -> Set
{ leaf : [i : Size] -> Tree A ($ i)
; node : [i : Size] -> A -> Tree A i -> Tree A i -> Tree A ($ i)
}

-- this definition is fine since the result type is a product
-- where each of its components is coinductive in i (TLCA, 2003)
cofun lab : [i : Size] -> [A : Set] -> [B : Set] ->
   Tree A i -> Stream (Stream B #) i ->
   Prod (Tree B i) (Stream (Stream B #) i)
{}
-- ...




{-
    Measure Termination
    ===================

  fun ack : [i, j : Size] -> |i,j| -> Nat i -> Nat j -> Nat #
  { ack .$i j   (zero i)    m         =
     succ # m

  ; ack .$i .$j (succ i n) (zero j)   =
     ack i # n (succ # (zero #))

  ; ack .$i .$j (succ i n) (succ j m) =
     ack i # n (ack $i j (succ i n) m)
  }

  RHS: ack : [i', j' : Size] -> |i',j'| < |$i,$j| ->
             Nat i' -> Nat j' -> Nat #

  Goals : |i ,#| < |$i,$j|
          |$i,j| < |$i,$j|

Mutual recursion:

  even : Nat -> Bool
  even n = even' n

  even' : Nat -> Bool
  even' zero    = true
  even' (suc n) = odd n

  odd : Nat -> Bool
  odd zero    = false
  odd (suc n) = even n
-}

data Bool : Set
{ true : Bool
; false : Bool
}

sized data Nat : Size -> Set
{ zero : [i : Size] -> Nat $i
; succ : [i : Size] -> Nat i -> Nat $i
}

mutual {

  fun even  : [i : Size] -> |i,$0| -> Nat i -> Bool
  { even i n = even' i n
  }

  fun even' : [i : Size] -> |i,0|  -> Nat i -> Bool
  { even' i (zero (i > j))   = true
  ; even' i (succ (i > j) n) = odd' j n
  }

  fun odd'  : [i : Size] -> |i,0|  -> Nat i -> Bool
  { odd' i (zero (i > j))   = false
  ; odd' i (succ (i > j) n) = even j n
  }
}

{-
    Bounded Quantification
    ======================

Stack objects as record:

  data Stack a = Stack
    { top  :: Maybe a
    , pop  :: Stack a
    , push :: a -> Stack a
    }

Constructing the empty stack needs a circular program.

  push' :: Stack a -> a -> Stack a
  push' s a = s'
    where s' = Stack (Just a) s (push' s')

  empty :: Stack a
  empty = Stack Nothing empty (push' empty)

-}

data Maybe (A : Set) : Set
{ nothing : Maybe A
; just : A -> Maybe A
}

-- stack object
sized codata Stack (A : Set) : Size -> Set
{ stack : [i : Size] ->
  (top  : Maybe A) ->
  (pop  : Stack A i) ->
  (push : A -> Stack A i) -> Stack A $i
}
fields top, pop, push

-- functional to construct push action
cofun pushFunc : [A : Set] -> [i : Size] -> |i| ->
                 ([j : Size] -> |j| < |i| -> Stack A j -> A -> Stack A j) ->
                 Stack A i -> A -> Stack A i
{ pushFunc A ($ i) f s a = stack i
   (just a)
   s
   (f i (pushFunc A i f s a))
}
-- f : [j : Size] -> |j| < |$i| -> Stack A j -> A -> Stack A j
-- s : Stack A $i
-- by subtyping
-- f : [j : Size] -> |j| < |i| -> Stack A j -> A -> Stack A j
-- s : Stack A i
-- hence  pushFunc A i f s a : Stack A i
--   f i (...) : A -> Stack A i
-- rhs : Stack A $i

-- tying the knot
cofun pushFix  : [A : Set] -> [i : Size] -> |i| -> Stack A i -> A -> Stack A i
{ pushFix A ($ i) = pushFunc A ($ i) (pushFix A)
}
-- on the rhs, we have the typing of the recursive call
--   pushFix A : [j : Size] -> |j| < |$i| -> Stack A j -> A -> Stack A j

-- constructing the empty stack
cofun empty : [A : Set] -> [i : Size] -> |i| -> Stack A i
{ empty A ($ i) = stack i nothing (empty A i) (pushFix A i (empty A i))
}

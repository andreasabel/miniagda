{- 2010-09-15, Agda Issue 276, posted by Conor

module RecConBug where

boo : {S T : Set}(f : S -> T)(x y : S) ->
      ((P : S -> Set) -> P x -> P y) ->
       (P : T -> Set) -> P (f x) -> P (f y)
boo = \ f x y q P -> q (\ s -> P (f s))
-}

let 
  boo : [S, T : Set] -> (f : S -> T) -> (x,  y : S) ->
        ((P : S -> Set) -> P x -> P y) ->
        (P : T -> Set) -> P (f x) -> P (f y)
  = \ S T f x y q P -> q (\ s -> P (f s))

{-
record Pack (S : Set) : Set where
  constructor pack
  field
    unpack : S

open Pack

unpack' : {S : Set} -> Pack S -> S
unpack' (pack s) = s
-}

data Pack (S : Set) : Set
{ pack : (unpack : S) -> Pack S
}
fields unpack

fun unpack' : [S : Set] -> Pack S -> S
{ unpack' S (pack .S s) = s
}

{-
foo : {S : Set}(x : Pack S)(P : Pack S -> Set) -> P (pack (unpack x)) -> P x
foo = \ x P p -> p

goo : {S : Set}(x : Pack S)(P : S -> Set) -> P (unpack x) -> P (unpack' x)
goo = \ x -> boo unpack' (pack (unpack x)) x (foo x)
-}

let foo : [S : Set] -> (x : Pack S) -> (P : Pack S -> Set) -> 
          P (pack S (unpack S x)) -> P x
        = \ S x P p -> p

let goo : [S : Set] -> (x : Pack S) -> (P : S -> Set) -> P (unpack S x) -> P (unpack' S x)
        = \ S x -> boo (Pack S) S (unpack' S) (pack S (unpack S x)) x (foo S x)

{- normal form of goo is \ x P p -> p 

goo' : {S : Set}(x : Pack S)(P : S -> Set) -> P (unpack x) -> P (unpack' x)
goo' = \ x P p -> p

/Users/conor/Desktop/fooling/RecConBug.agda:27,19-20
unpack x != unpack' x of type .S
when checking that the expression p has type P (unpack' x)
-}


let goo' : [S : Set] -> (x : Pack S) -> (P : S -> Set) -> 
           P (unpack S x) -> P (unpack' S x)
         = \ S x P p -> p

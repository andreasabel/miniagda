-- 2011-04-04 Gothenburg
-- see www.cse.chalmers.se/~nad/listings/lib/README.Record.html

let prop : Set 1 = Set 0

data PER : Set 1 
{ per : (S : Set 0) ->
        (R : S -> S -> prop) ->
        (sym : (x : S) -> R x y -> R y x) ->
        (trans : (x, y, z : S) -> R x y -> R y z -> R x z) ->
        PER
} fields S, R, sym, trans

{- NEED to implement With, but this only works if we have a intensional
   description of Records

fun converse : (P : PER) -> P With S = P.S and R = flip P.R

-}
-- Girard's System U^-
-- Author: Andreas Abel
-- Created: 2004-12-06
-- Modified: 2010-06-30 port to MiniAgda
-- Source:
-- Coquand/Barthe "Equational Theory of Non-Normalizing PTS" (buggy)
-- Dybjer: http://www.cse.chalmers.se/~peterd/kurser/tt03/project.ps


-- sorts and axioms

let triangle : Set 3    = Set 2
let box      : triangle = Set 1
let star     : box      = Set 0

-- rules

-- (->) : star -> star -> star -- automatically
-- (->) : box -> box -> box    -- automatically

-- two impredicative universes

-- (->) : box -> star -> star
-- (->) : triangle -> box -> box

data False : Set {} -- : star

-- powerset
let P : box -> box
    = \ X -> X -> star

let not : star -> star
    = \ phi -> phi -> False

-- Definition of a paradoxical universe

trustme -- here is the invalid impredicativity
let U : box
    = [X : box] -> (P (P X) -> X) -> P (P X)  -- uses (triangle,box,box)

let tau : P (P U) -> U
    = \ t X g p -> t (\ x -> p (g (x X g)))

let sigma : U -> P (P U)
    = \ s -> s U tau

let rho : U -> U
    = \ y -> tau (sigma y)

-- Intermediate definitions

impredicative
let Q : P (P U)                           -- Q : (U -> star) -> star
    = \ p -> [x : U] -> sigma x p -> p x  -- uses (box,star,star)

impredicative
let B : P U
    = \ y -> not ([p : P U] -> sigma y p -> p (rho y))  -- (box,star,star)

let C : U
    = tau Q

impredicative
let D : star
    = [p : P U] -> Q p -> p C  -- (box,star,star)

let M : Q B
    = \ x k l -> l B k (\ p -> l (\ y -> p (rho y)))

let L : not D
    = \ d -> d B M (\ p -> d (\ y -> p (rho y)))

let R : D
    = \ p h -> h C (\ x -> h (rho x))

-- eval
let loop : False
    = L R

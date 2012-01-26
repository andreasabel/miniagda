-- 2010-09-14, example put forward by Nisse

data Bool : Set
{ true  : Bool
; false : Bool
}

data Prod ++(A, B : Set) : Set
{ pair : (fst : A) -> (snd : B) -> Prod A B }
fields fst, snd

data Eq (i : Size)(A : Set i)(a : A) : A -> Set i
{ refl : Eq i A a a }

-- if I put trustme here, it says it ignored an error, but 
-- without trustme there is no error !?
fun D : [A : Set] -> (p1, p2 : Prod A A) -> 
        Eq 0 A (fst p1) (fst p2) -> Set 1
{ D A (pair x y) (pair .x y') (refl) = Set 
}  -- miniagda has problem typechecking this !?

fun prf : [A : Set] -> (p : Prod A A) -> 
          Eq 2 (Set 1) (D A p p (refl {- 0 A (fst A A p) -})) Set
{ prf A p = refl -- 2 (Set 1) Set
}

{- 

* At the moment I remove dot patterns inside record patterns. This seems
 like a bad idea. Consider the following example:

   D : (p1 p2 : A × A) -> proj1 p1 == proj1 p2 -> Set1
   D (x , y) (.x , y') refl = Set

 The translated clause is not well-typed:

   D p p' refl = Set

 What should we do about this? We cannot leave the clause as is:
 D p p refl should reduce. On the other hand I don't see any simple
 translation for it. The following code seems too complicated to me:

   D : (p1 p2 : B) -> proj1 p1 == proj1 p2 -> Set1
   D p1 p2 eq with proj1 p1 | proj2 p1 | proj1 p2 | proj2 p2
   D p1 p2 refl | x | y | .x | y' = Set

 Perhaps record patterns containing dot patterns should be rejected.
 However, in this case we should tell Agda's magic algorithm for
 inserting dots to avoid inserting dots inside record patterns whenever
 possible. Is this feasible?

--
/NAD
-}
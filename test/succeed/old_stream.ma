-- Booleans ----------------------------------------------------------

data Bool : Set 
{ tt : Bool
; ff : Bool
}

fun ifthenelse : Bool -> (A : Set) -> A -> A -> A
{ ifthenelse tt A a1 a2 = a1
; ifthenelse ff A a1 a2 = a2
}

-- Nat ---------------------------------------------------------------

data Nat : Set 
{ zero : Nat
; succ : Nat -> Nat 
}

fun add : Nat -> Nat -> Nat 
{ add zero     = \ y -> y
; add (succ x) = \ y -> succ (add x y)
}

fun leq : Nat -> Nat -> Bool
{ leq  zero     y       = tt
; leq (succ x)  zero    = ff 
; leq (succ x) (succ y) = leq x y 
}

-- Stream ------------------------------------------------------------

sized codata Stream (+ A : Set) : Size -> Set 
{ cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}
 
fun tail : (A : Set) -> (i : Size) -> Stream A ($ i) -> Stream A i
{ tail A i (cons .A .i x xs) = xs
}

fun head : (A : Set) -> (i : Size) -> Stream A ($ i) -> A 
{ head A i (cons .A .i x xs) = x
}

fun nth : Nat -> Stream Nat # -> Nat 
{ nth zero xs     = head Nat # xs
; nth (succ x) xs = nth x (tail Nat # xs) 
}

-- map, zip, merge ---------------------------------------------------

cofun map : (A : Set) -> (B : Set) -> (i : Size) -> 
            (A -> B) -> Stream A i -> Stream B i 
{
map A B ($ i) f (cons .A .i x xl) = cons B _ (f x) (map A B _ f xl)
}

cofun zipWith : (A : Set) -> (B : Set) -> (C : Set) ->
                (A -> B -> C) -> (i : Size) ->
		Stream A i -> Stream B i -> Stream C i 
{
  zipWith A B C f ($ i) (cons .A .i a as) (cons .B .i b bs) = 
	cons C i (f a b)  (zipWith A B C f i as bs) 
}

cofun merge : (i : Size) -> (Nat -> Nat -> Bool) -> 
              Stream Nat i -> Stream Nat i -> Stream Nat i
{
merge ($ i) le (cons .Nat .i x xs) (cons .Nat .i y ys) = 
      ifthenelse (le x y) (Stream Nat _)
         (cons Nat _ x (merge _ le xs (cons Nat _ y ys)))
	 (cons Nat _ y (merge _ le (cons Nat _ x xs) ys))     
}

{-
cofun merge : (i : Size) -> (Nat -> Nat -> Bool) -> 
              Stream Nat i -> Stream Nat i -> Stream Nat i
{
merge .($ i) le (cons Nat .i x xs) (cons Nat i y ys) = 
      ifthenelse (le x y) (Stream Nat _)
         (cons Nat _ x (merge _ le xs (cons Nat _ y ys)))
	 (cons Nat _ y (merge _ le (cons Nat _ x xs) ys))     
}
-}

-- Hamming function --------------------------------------------------

let one   : Nat = succ zero
let two   : Nat = succ one
let three : Nat = succ two
let four  : Nat = succ three
let five  : Nat = succ four

let double : Nat -> Nat
           = \ n -> add n n
let triple : Nat -> Nat
           = \ n -> add n (double n)

cofun ham : (i : Size) -> Stream Nat i
{
  ham ($ i) = cons Nat _ one (merge i leq (map Nat Nat i double (ham i)) 
                                    (map Nat Nat i triple (ham i)))
}


{-
-- THIS SHOULD NOT TYPECHECK!!
cofun map2 : (i : Size) -> (Nat -> Nat) -> Stream Nat i -> Stream Nat i 
{
map2 .($ ($ i)) f (cons .Nat .($ i) u (cons .Nat i x xl)) = 
  cons Nat _ (f u) (cons Nat _ (f x) (map2 _ f xl))
}

cofun ham2 : (i : Size) -> Stream Nat i
{
  ham2 ($ i) = cons Nat _ one (merge i leq (map2 i double (ham2 i)) 
                                     (map2 i triple (ham2 i)))
}

-- THIS LOOPS!!!
eval let bla : Nat = nth one (ham2 #)
-}

-- Fibonacci stream --------------------------------------------------

{- NOT YET IMPLEMENTED: rational sizes
   WILL NOT IMPLEMENT -- see fibDeep.ma

cofun fib : (i : Size) -> Stream Nat (i + i)
{
  fib (i + 1) = cons Nat _ zero (cons Nat _ one (zipWith Nat Nat Nat add
    i (fib i) (tail Nat i (fib (i + 1/2)))))
}

-}

{- distinguish fib from the following

cofun bad : [i : Size] -> Stream Nat i
{
  bad ($ ($ i)) = cons Nat _ zero (tail Nat _ (bad ($ i)))
}

-}

cofun fib : (i : Size) -> Stream Nat i
{
  fib ($ i) = cons Nat _ zero (zipWith Nat Nat Nat add i 
    (cons Nat _ one (fib i)) (fib i))
}



cofun fibIter' : (x : Nat ) -> (y : Nat ) -> (i : Size) -> Stream Nat i 
{
  fibIter' x y ($ i) = cons Nat _ x (fibIter' y (add x y) _)
} 
let fibIter : Stream Nat # = (fibIter' one one _)


--------------------------------------------

-- fibIter(4) = 5 
eval let fibIter4 : Nat = nth four fibIter 

eval let fib1 : Nat = nth one   (fib #)
eval let fib2 : Nat = nth two   (fib #)
eval let fib3 : Nat = nth three (fib #)
eval let fib4 : Nat = nth four  (fib #)
eval let fib5 : Nat = nth five  (fib #)


--------------------------------------------

data Leq : Nat -> Nat -> Set
{
lqz : (x : Nat ) -> Leq zero x ;
lqs : (x : Nat ) -> (y : Nat ) -> Leq x y -> Leq (succ x) (succ y)
}

sized codata Increasing : Size -> Stream Nat # -> Set
{
inc : (i : Size ) -> (x : Nat ) -> (y : Nat ) -> Leq x y -> (tl : Stream Nat # ) -> 
      Increasing i (cons Nat # y tl) ->
      Increasing ($ i) (cons Nat # x (cons Nat # y tl)) 
}


data Eq (+ A : Set ) : A -> A -> Set
{
refl : (a : A ) -> Eq A a a
}

let proof : Eq (Stream Nat #) (tail Nat # fibIter) (tail Nat # fibIter) = refl (Stream Nat _) (tail Nat # fibIter)




cofun evil : (i : Size ) -> Stream Nat i
{
evil ($ i) = map Nat Nat _ succ (cons Nat _ zero (evil _))
}

-- eval const zzz : Nat = head # (z #) 



-- convolution (Shin-Cheng Mu)
 
cofun dmerge : (A : Set) -> (i : Size) -> Stream (Stream A i) i -> Stream A i
{
dmerge A ($ i) (cons .(Stream A ($ i)) .i ys yss) = 
  cons A i (head A _ ys) (dmerge A i
    (zipWith A (Stream A _) (Stream A _) (cons A _) i 
            (tail A _ ys) yss))
}



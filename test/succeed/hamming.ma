-- Nat ---------------------------------------------------------------

data Nat : Set 
{ zero : Nat
; succ : Nat -> Nat 
}

fun add : Nat -> Nat -> Nat 
{ add  zero    = \y -> y
; add (succ x) = \y -> succ (add x y)
}

let double : Nat -> Nat
           = \ n -> add n n
let triple : Nat -> Nat
           = \ n -> add n (double n)

fun leq : Nat -> Nat -> [C : Set] -> C -> C -> C
{ leq  zero     y       C tt ff = tt
; leq (succ x)  zero    C tt ff = ff
; leq (succ x) (succ y) C tt ff = leq x y C tt ff 
}

-- Stream ------------------------------------------------------------

sized codata Stream (+ A : Set) : Size -> Set 
{
  cons : [i : Size] -> A -> Stream A i -> Stream A ($ i)
}

cofun map : [A : Set] -> [B : Set] -> [i : Size] -> 
            (A -> B) -> Stream A i -> Stream B i 
{
  map A B ($ i) f (cons .A .i x xl) = cons B _ (f x) (map A B _ f xl)
}

cofun merge : [i : Size] -> Stream Nat i -> Stream Nat i -> Stream Nat i
{
  merge ($ i) (cons .Nat .i x xs) (cons .Nat .i y ys) = 
      leq x y (Stream Nat _)
         (cons Nat _ x (merge _ xs (cons Nat _ y ys)))
	 (cons Nat _ y (merge _ (cons Nat _ x xs) ys))     
}


-- Hamming function --------------------------------------------------

cofun ham : [i : Size] -> Stream Nat i
{
  ham ($ i) = cons Nat _ (succ zero) 
                (merge i (map Nat Nat i double (ham i)) 
                         (map Nat Nat i triple (ham i)))
}

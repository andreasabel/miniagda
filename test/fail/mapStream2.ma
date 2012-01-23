
sized codata Stream (+ A : Set) : Size -> Set {
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}
 
data Nat : Set {
  zero : Nat;
  succ : Nat -> Nat 
}

-- THIS SHOULD NOT TYPECHECK!!
cofun map2 : (i : Size) -> (Nat -> Nat) -> Stream Nat i -> Stream Nat i 
{
map2 .($ ($ i)) f (cons .($ i) u (cons i x xl)) = 
  cons _ (f u) (cons _ (f x) (map2 _ f xl))
}

{- a better explanation why this does not work:

- the quantification  (i : Size) -> ... Stream Nat i  is a CoSize quant.
- disallow dot patterns for CoSize 

cofun map2 : (i : Size) -> (Nat -> Nat) -> Stream Nat i -> Stream Nat i 
{
map2 ($ ($ i)) f (cons .Nat .($ i) u (cons .Nat .i x xl)) = 
  cons Nat _ (f u) (cons Nat _ (f x) (map2 _ f xl))
}

- this then fails since deep matching is not allowed
- for the CoSizes inside the cons we would still have to allow dot patterns
- how to separate these two uses?
- maybe: the size pattern inside cocons can only be a dot pattern?!
-}
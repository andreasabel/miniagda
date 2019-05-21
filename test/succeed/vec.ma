data Nat : Set
{
  zero : Nat;
  suc : (pred : Nat) -> Nat
}

fun add : Nat -> Nat -> Nat
{
  add zero y = y;
  add (suc x) y = suc (add x y)
}

data Vec' (+A : Set) : Nat -> Set
{
  vnil'  : Vec' A zero;
  vcons' :  [n : Nat] -> (head' : A) -> (tail' : Vec' A n) -> Vec' A (suc n)
}

data Vec (+A : Set) : Nat -> Set
{
  vnil  : Vec A zero;
  vcons : (head : A) -> [n : Nat] -> (tail : Vec A n) -> Vec A (suc n)
} fields head, tail

fun length : [A : Set] -> [n : Nat] -> Vec A n -> Nat
{
  length A .zero vnil = zero;
  length A .(suc n) (vcons x n xs) = suc (length A n xs);
}

fun append : [A : Set] -> [n : Nat] -> Vec A n ->
                          [m : Nat] -> Vec A m -> Vec A (add n m)
{
  append A .zero     vnil         m ys = ys;
  append A .(suc n) (vcons x n xs) m ys =
    vcons x (add n m) (append A n xs m ys)
}

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

let vec0vnil : (A : Set) -> (v : Vec A zero) -> Id (Vec A zero) v vnil
             = \ A -> \ v -> refl

-- -- No eta for recursive constructors like vcons yet.
-- let vecSucVCons : [A : Set] -> [n : Nat] -> (v : Vec A (suc n))
--   -> Id (Vec A (suc n)) v (vcons (head v) n (tail v))
--   = \ A -> \ n -> \ v -> refl

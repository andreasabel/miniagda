-- 2010-03-29
-- Nisse, "Beating the productivity checker" Draft 2010
-- pp. 4, 9

-- Leibniz equality

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}      

fun subst : [A : Set] -> [P : A -> Set] -> [a : A] -> [b : A] -> Id A a b -> P a -> P b
{ subst A P a .a (refl .A .a) x = x
} 

fun cong : [A : Set] -> [B : Set] -> [f : A -> B] -> [a : A] -> [b : A] -> 
  Id A a b -> Id B (f a) (f b)
{ cong A B f a .a (refl .A .a) = refl B (f a)
}

fun trans : [A : Set] -> [a : A] -> [b : A] -> [c : A] -> Id A a b -> Id A b c -> Id A a c
{ trans A a .a .a (refl .A .a) (refl .A .a) = refl A a 
}

-- Streams

sized codata Stream (A : Set) : Size -> Set
{ cons : [i : Size] -> A -> Stream A i -> Stream A ($ i)
}

cofun iterate : [A : Set] -> [i : Size] -> (f : A -> A) -> A -> Stream A i
{ iterate A ($ i) f a = cons A i a (iterate A i f (f a))
}

cofun map : [A : Set] -> [B : Set] -> (f : A -> B) -> 
            [i : Size] -> Stream A i -> Stream B i
{ map A B f ($ i) (cons .A .i a as) = cons B i (f a) (map A B f i as)
} 

-- Bisimulation

sized codata Bisim (A : Set) : Size -> Stream A # -> Stream A # -> Set
{ eq : [i : Size] -> (x : A) -> (xs : Stream A #) -> (ys : Stream A #) ->
       Bisim A i xs ys -> Bisim A ($ i) (cons A # x xs) (cons A # x ys)
}

cofun rfl : [A : Set] -> [i : Size] -> (xs : Stream A #) -> Bisim A i xs xs
{ rfl A ($ i) (cons .A .# x xs) = eq A i x xs xs (rfl A i xs)
}

fun emb : [A : Set] -> [i : Size] -> 
  (xs : Stream A #) -> (ys : Stream A #) -> Id (Stream A #) xs ys -> Bisim A i xs ys
{ emb A i xs .xs (refl .(Stream A #) .xs) = rfl A i xs
}

cofun tra : [A : Set] -> [i : Size] -> 
  [xs : Stream A #] -> [ys : Stream A #] -> [zs : Stream A #] ->
  Bisim A i xs ys -> Bisim A i ys zs -> Bisim A i xs zs
{ tra A ($ i) .(cons A # x xs) .(cons A # x ys) .(cons A # x zs) 
    (eq .A .i x xs ys hxy) (eq .A .i .x .ys zs hyz) =
    eq A i x xs zs (tra A i xs ys zs hxy hyz)
}

-- Lemmas about map and iterate

cofun mapIter : [A : Set] -> [i : Size] -> (f : A -> A) -> (x : A) ->
      Bisim A i (map A A f # (iterate A # f x)) (iterate A # f (f x))
{ mapIter A ($ i) f x = eq A i (f x) 
     (map A A f # (iterate A # f (f x))) 
     (iterate A # f (f (f x))) 
   (mapIter A i f (f x))
}

cofun fusion : [A : Set] -> [B : Set] -> [i : Size] ->  
  (h : A -> B) -> (f1 : A -> A) -> (f2 : B -> B) ->
  ((x : A) -> Id B (h (f1 x)) (f2 (h x))) ->
  (x : A) -> Bisim B i (map A B h # (iterate A # f1 x)) (iterate B # f2 (h x))
{ fusion A B ($ i) h f1 f2 ass x = eq B i (h x) 
      (map A B h # (iterate A # f1 (f1 x)))
      (iterate B # f2 (f2 (h x)))
    (tra B i 
         (map A B h # (iterate A # f1 (f1 x)))
         (iterate B # f2 (h (f1 x)))
         (iterate B # f2 (f2 (h x)))
      (fusion A B i h f1 f2 ass (f1 x))
      (emb B i 
         (iterate B # f2 (h (f1 x)))
         (iterate B # f2 (f2 (h x)))
        (cong B (Stream B #) (\ y -> iterate B # f2 y) (h (f1 x)) (f2 (h x)) (ass x)))
    )
} 
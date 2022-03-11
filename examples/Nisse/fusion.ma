-- 2010-03-29
-- Nisse, "Beating the productivity checker" Draft 2010
-- pp. 4, 9

-- Leibniz equality

data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

fun subst : [A : Set] -> [P : A -> Set] -> [a : A] -> [b : A] -> Id A a b -> P a -> P b
{ subst A P a .a refl x = x
}

fun cong : [A : Set] -> [B : Set] -> [f : A -> B] -> [a : A] -> [b : A] ->
  Id A a b -> Id B (f a) (f b)
{ cong A B f a .a refl = refl
}

fun trans : [A : Set] -> [a : A] -> [b : A] -> [c : A] -> Id A a b -> Id A b c -> Id A a c
{ trans A a .a .a refl refl = refl
}

-- Streams

sized codata Stream (A : Set) : Size -> Set
{ cons : [i : Size] -> A -> Stream A i -> Stream A ($ i)
}

cofun iterate : [A : Set] -> [i : Size] -> (f : A -> A) -> A -> Stream A i
{ iterate A ($ i) f a = cons i a (iterate A i f (f a))
}

cofun map : [A : Set] -> [B : Set] -> (f : A -> B) ->
            [i : Size] -> Stream A i -> Stream B i
{ map A B f ($ i) (cons .i a as) = cons i (f a) (map A B f i as)
}

-- Bisimulation

sized codata Bisim (A : Set) : Size -> Stream A # -> Stream A # -> Set
{ eq : [i : Size] -> (x : A) -> (xs : Stream A #) -> (ys : Stream A #) ->
       Bisim A i xs ys -> Bisim A ($ i) (cons # x xs) (cons # x ys)
}

cofun rfl : [A : Set] -> [i : Size] -> (xs : Stream A #) -> Bisim A i xs xs
{ rfl A ($ i) (cons .# x xs) = eq i x xs xs (rfl A i xs)
}

fun emb : [A : Set] -> [i : Size] ->
  (xs : Stream A #) -> (ys : Stream A #) -> Id (Stream A #) xs ys -> Bisim A i xs ys
{ emb A i xs .xs refl = rfl A i xs
}

cofun tra : [A : Set] -> [i : Size] ->
  [xs : Stream A #] -> [ys : Stream A #] -> [zs : Stream A #] ->
  Bisim A i xs ys -> Bisim A i ys zs -> Bisim A i xs zs
{ tra A ($ i) .(cons # x xs) .(cons # x ys) .(cons # x zs)
    (eq .i x xs ys hxy) (eq .i .x .ys zs hyz) =
    eq i x xs zs (tra A i xs ys zs hxy hyz)
}

-- Lemmas about map and iterate

cofun mapIter : [A : Set] -> [i : Size] -> (f : A -> A) -> (x : A) ->
      Bisim A i (map A A f # (iterate A # f x)) (iterate A # f (f x))
{ mapIter A ($ i) f x = eq i (f x)
     (map A A f # (iterate A # f (f x)))
     (iterate A # f (f (f x)))
   (mapIter A i f (f x))
}

cofun fusion : [A : Set] -> [B : Set] -> [i : Size] ->
  (h : A -> B) -> (f1 : A -> A) -> (f2 : B -> B) ->
  ((x : A) -> Id B (h (f1 x)) (f2 (h x))) ->
  (x : A) -> Bisim B i (map A B h # (iterate A # f1 x)) (iterate B # f2 (h x))
{ fusion A B ($ i) h f1 f2 ass x = eq i (h x)
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

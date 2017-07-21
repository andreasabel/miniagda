sized codata Stream (A : Set) : Size -> Set
{ cons : [i : Size] -> (head : A) -> (tail : Stream A i) -> Stream A $i
} fields head, tail

cofun interleave : [A : Set] -> [i : Size] -> Stream A i -> Stream A i
-> Stream A (i + i)
{ interleave A ($ i) (cons .i x xs) (cons .i y ys) =
    cons $(i + i) x (cons (i + i) y (interleave A i xs ys))
}

cofun evens : [A : Set] -> [i : Size] -> Stream A (i + i) -> Stream A i
{ evens A ($i) (cons .(i + i + 1) a (cons .(i + i) b as)) =
   cons i a (evens A i as)
}

cofun odds : [A : Set] -> [i : Size] -> Stream A (i + i) -> Stream A i
{ odds A ($i) (cons .(i + i + 1) a (cons .(i + i) b as)) =
   cons i b (odds A i as)
}

let weave : [A : Set] -> [i : Size] -> Stream A (i + i) -> Stream A (i + i)
  = \ A i xs -> interleave A i (evens A i xs) (odds A i xs)

cofun map : [A, B : Set] -> (A -> B) -> [i : Size] -> Stream A i -> Stream B i
{ map A B f ($ i) (cons .i a as) = cons i (f a) (map A B f i as)
}

data Nat : Set
{ zero : Nat
; succ : Nat -> Nat
}

{-
cofun nats : [i : Size] -> Stream Nat i
{ nats $i =
   let nats' : Stream Nat i
             = map Nat Nat succ i (nats i)
   in cons Nat i zero (interleave Nat (evens nats') (odds nats'))
}
-}

{- 2011-04-08
daniel.james@comlab.ox.ac.uk

Hi Andreas,

I have a question regarding MiniAdga. I noticed that you have started
to add support for size addition.

You have the following definition on your blog:

cofun evens : [A : Set] -> [i : Size] -> Stream A (i + i) -> Stream A i
{ evens A ($i) (cons .(i + i + 1) a (cons .(i + i) b as)) =
   cons i a (evens A i as)
}

Would it also be possible have something like,

cofun interleave : [A : Set] -> [i : Size] -> Stream A i -> Stream A i
-> Stream A (i + i)

where interleave (\/) is defined as,

s \/ t = head s ::  t \/ tail s

such that MiniAgda would accept the following alternative definition
of the stream of natural numbers?

nats = 0 :: evens (nats + 1) \/ odds (nats + 1)

-}

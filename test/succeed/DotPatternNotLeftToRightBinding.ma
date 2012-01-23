-- 2010-09-22
-- 2012-01-22 parameters gone from constructors

fun A : Set {}
fun B : Set {}
fun f : A -> A {}

data Fix *(a : A) : A -> Set
{ fix : Fix a (f a)
}

-- eta not definable unconditionally (like for Id)
fun eta : (a, b : A) -> Fix a b -> Fix a b
{ eta a .(f a) (fix) = fix
} 

-- variable a used in dot pattern left of its binding
fun bla : (b, a : A) -> Fix a b -> A
{ bla .(f a) a (fix) = a
} 

-- Function inverse

data Inv (g : A -> B) : B -> Set
{ mkInv : (getInv : A) -> Inv g (g getInv)
}
-- MiniAgda does not generate destructor getInv

fun getInv : (g : A -> B) -> (b : B) -> Inv g b -> A
  { getInv g .(g a) (mkInv a) = a 
  }

{- Analysis:

  mkInv : (getInv : A) -> Inv g b  where b = (g getInv)

bind b in destructor type after parameters

  getInv : (g : A -> B) -> (b : B) -> Inv g b -> A

put its value (g a) down as dot-pattern instead of b

  getInv g .(g a) (mkInv .g a) = a

-}

{-
data Id (A : Set)(a : A) : A -> Set
{ refl : Id A a a
}

fun f : (A : Set) -> (c : A -> A) -> (a : A) -> (b : A) -> Id A (c b) b -> A
{ f A c .(c b) b (refl .A .b) = b
}
-}
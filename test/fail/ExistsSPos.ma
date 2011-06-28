-- 2010-06-11, Nisse

data Exists (A : Set) (+ B : A -> Set) : Set
{ exI : (witness : A) -> (proof : B witness) -> Exists A B
}

data Foo : Set
{ foo : Exists Foo (\ x -> Foo) -> Foo
}

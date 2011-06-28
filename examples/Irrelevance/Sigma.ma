-- 2010-10-01

-- ordinary Sigma types

data Sigma (A : Set) (B : A -> Set) : Set
{ pair : (fst : A) -> (snd : B fst) -> Sigma A B
}

fun split : [A : Set] -> [B : A -> Set] -> (x : Sigma A B) ->
            [C : Sigma A B -> Set] -> 
            ((fst : A) -> (snd : B fst) -> C (pair A B fst snd)) ->
            C x
{ split A B (pair .A .B a b) C k = k a b
}

-- define projections in terms of split

fun fst : [A : Set] -> [B : A -> Set] -> Sigma A B -> A
{ fst A B x = split A B x (\ y -> A) (\ a b -> a)
}

fun snd : [A : Set] -> [B : A -> Set] -> (x : Sigma A B) -> B (fst A B x)
{ snd A B x = split A B x (\ y -> B (fst A B y)) (\ a b -> b)
}

-- existential types

data Exists (A : Set) (B : A -> Set) : Set
{ expair : [exfst : A] -> (exsnd : B exfst) -> Exists A B
}

fun exsplit : [A : Set] -> [B : A -> Set] -> (x : Exists A B) ->
              [C : Exists A B -> Set] -> 
              ([fst : A] -> (snd : B fst) -> C (expair A B fst snd)) ->
              C x
{ exsplit A B (expair .A .B a b) C k = k a b
}

-- first projection not definable
trustme fun exfst : [A : Set] -> [B : A -> Set] -> Exists A B -> A
{ exfst A B x = exsplit A B x (\ y -> A) (\ a b -> a)
}

-- second projection would be definable, if first projection was available
fun exsnd : [A : Set] -> [B : A -> Set] -> (x : Exists A B) -> B (exfst A B x)
{ exsnd A B x = exsplit A B x (\ y -> B (exfst A B y)) (\ a b -> b)
}

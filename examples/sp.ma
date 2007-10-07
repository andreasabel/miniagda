
codata Stream (A : Set) : Size -> Set 
{
  cons : (i : Size) -> A -> Stream A i -> Stream A ($ i)
}
 

data ISP ( A : Set ) ( B : Set ) (K : Set) : Set
{
put : B -> K -> ISP A B K; 
get : (A -> ISP A B K) -> ISP A B K; 
}


-- sized codata SP (A : Set) (B : Set) : Set
--   sp : ISP A B (SP A B) -> SP A B
--
codata SP (A : Set) (B : Set) : Size -> Set
{
sp : (i : Size) -> ISP A B (SP A B i) -> SP A B ($ i);
}

fun ieat : (A : Set) -> (B : Set) ->  (K : Set) -> (C : Set) -> 
           ISP A B K -> Stream A # -> (B -> K -> Stream A # -> C) -> C 
{
ieat .A .B .K C (get .A B K f) (cons A .# a as) h = 
  ieat A B K C (f a) as h ;
ieat .A .B .K C (put A B K b k)              as  h = h b k as 
}

cofun eat : (i : Size) -> (A : Set) -> (B : Set) -> 
            SP A B # -> Stream A # -> Stream B i
{
eat ($ i) .A .B (sp A B .# isp) as 
  = ieat A B (SP A B #) (Stream B ($ i))
                                    isp as (\ b -> \ k -> \ as' -> 
                                            cons B i b (eat i A B k as'))   
}



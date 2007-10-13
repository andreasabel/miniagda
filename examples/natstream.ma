data Nat : Set
{
zero : Nat;
succ : Nat -> Nat;
}

codata Stream : Set
{
cons : Nat -> Stream -> Stream
}

codata Eq : Stream -> Stream -> Set
{
eq : ( n : Nat ) -> (s1 : Stream) -> (s2 : Stream ) -> Eq s1 s2 -> Eq (cons n s1) (cons n s2)
}

cofun trans : ( s1 : Stream ) -> (s2 : Stream ) -> (s3 : Stream ) -> Eq s1 s2 -> Eq s2 s3 -> Eq s1 s3
{ 
trans .(cons m s1) .(cons m s2) .(cons m s3) (eq .m s1 .s2 p) (eq m s2 s3 q) = 
	eq m s1 s3 (trans s1 s2 s3 p q)
}

cofun nats : Nat -> Stream
{
nats n = cons n (nats (succ n))
}

cofun map : (Nat -> Nat ) -> Stream -> Stream
{
map f (cons n l) = cons (f n) (map f l); 
}

cofun proof : (n : Nat ) -> ( l : Stream ) -> Eq l (cons n (map succ l)) -> Eq l (nats n)
{
proof .n .(cons n l) (eq n l .(cons (succ n) (map succ l)) h) = 
	eq n l (nats (succ n)) (proof (succ n) l h)
}

-- performance test

data Nat : Set 
{
  zero : Nat;
  succ : Nat -> Nat
}

fun h2 : Nat -> Nat -> Nat
{
  h2 zero zero = zero;
  h2 zero (succ y) = h2 zero y;
  h2 (succ x) y = h2 x y 
}

mutual {

fun  f : Nat -> Nat -> Nat
{
  f zero y = zero;
  f (succ x) zero = zero;
  f (succ x) (succ y) = h ( g x (succ y)) ( f (succ (succ (succ x))) y)
}

fun  g : Nat -> Nat -> Nat
{
  g zero y = zero;
  g (succ x) zero = zero;
  g (succ x) (succ y) = h ( f (succ x) (succ y)) (g x (succ (succ y)))  
}

fun h : Nat -> Nat -> Nat
{
  h zero zero = zero;
  h zero (succ y) = h zero y;
  h (succ x) y = h x y 
}
}


-- wahlstedt example
mutual
{

fun  f0 : Nat -> Nat -> Nat -> Nat -> Nat -> Nat
{
  f0 x1 zero       x3 x4        x5 = zero;
  f0 x1 (succ x2)  x3 zero      x5 = zero;
  f0 x1 (succ x2)  x3 (succ x4) x5 = f1 x2 (succ x2) x4 (succ x4) (succ (succ x5))
}

fun f1 : Nat -> Nat -> Nat -> Nat -> Nat -> Nat
{
  f1 x1 x2 x3 x4 zero = zero;
  f1 x1 x2 x3 x4 (succ x5) = f2 x2 x1 x3 x4 x5
}
  
fun f2 : Nat -> Nat -> Nat -> Nat -> Nat -> Nat
{
  f2 x1 x2 zero        x4 x5 = zero  ;
  f2 x1 x2 (succ x3) zero zero = zero;
  f2 x1 x2 (succ x3) zero (succ x5) = f3 x1 x2 x3 zero x5;
  f2 x1 x2 (succ x3) (succ x4) zero = zero;
  f2 x1 x2 (succ x3) (succ x4) (succ x5) = f5 x1 x2 (succ x3) x4 x5
}
 
fun f3 : Nat -> Nat -> Nat -> Nat -> Nat -> Nat
{
  f3 x1 x2 x3 x4 zero = zero;
  f3 x1 x2 x3 x4 (succ x5) = f4 x1 x2 x4 x3 x5
}

fun f4 : Nat -> Nat -> Nat -> Nat -> Nat -> Nat
{ 
  f4 zero        x2 x3 x4 x5 = zero  ;
  f4 (succ x1) zero x3 x4 zero = zero  ;
  f4 (succ x1) zero x3 x4 (succ x5)  = f3 x1 zero x3 x4 x5;
  f4 (succ x1) (succ x2) x3 x4 zero = zero;
  f4 (succ x1) (succ x2) x3 x4 (succ x5) = f2 x1 x2 x3 x4 x5
}


fun f5 : Nat -> Nat -> Nat -> Nat -> Nat -> Nat
{
  f5 x1 x2 x3 x4 zero = zero;
  f5 x1 x2 x3 x4 (succ x5) = f6 x2 x1 x3 x4 x5
}


fun f6 : Nat -> Nat -> Nat -> Nat -> Nat -> Nat
{
  f6 x1 x2 x3 x4 zero = zero;
  f6 x1 x2 x3 x4 (succ x5) = f0 x1 x2 x4 x3 x5
}
}